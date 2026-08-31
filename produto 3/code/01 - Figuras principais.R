#!/usr/bin/env Rscript
suppressPackageStartupMessages({library(data.table); library(MatchIt); library(did); library(ggplot2); library(readxl); library(openxlsx)})
set.seed(3854035)
args <- commandArgs(FALSE); script <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)][1]), winslash="/")
product <- normalizePath(file.path(dirname(script), ".."), winslash="/")
repo <- normalizePath(file.path(product, ".."), winslash="/")
data_dir <- file.path(repo,"data"); output_dir <- file.path(product,"analysis")
source(file.path(dirname(script), "_common.R"), encoding = "UTF-8")

units_file <- file.path(data_dir,"produto3_unidades_matching.csv")
old_units <- fread(units_file,sep=";",colClasses=c(cnpj_raiz="character"))
old_units[,cnpj_raiz:=clean_root(cnpj_raiz)]

# Refaz o matching principal. O pool é restrito aos municípios das tratadas;
# entre as especificações candidatas, escolhemos a que minimiza o maior
# desequilíbrio padronizado entre todas as covariadas.
matching_covars <- c("idade_firma_pre","prop_superior","tempo_medio_trabalhadores",
                    "idade_media_trabalhadores","prop_homens","prop_pretos")
covars_match <- fread(file.path(data_dir,"produto3_covariaveis_matching.csv"),sep=";",
                      colClasses=c(cnpj_raiz="character"))
covars_match[,cnpj_raiz:=clean_root(cnpj_raiz)]
smd <- function(d,v){
  a<-d[treat==1];b<-d[treat==0]
  ma<-weighted.mean(a[[v]],a$weights);mb<-weighted.mean(b[[v]],b$weights)
  den<-sqrt((weighted.mean((a[[v]]-ma)^2,a$weights)+weighted.mean((b[[v]]-mb)^2,b$weights))/2)
  ifelse(is.finite(den)&den>0,(ma-mb)/den,0)
}
matched_parts <- list()
matching_cohorts <- c(2024L,2025L)
for(g0 in matching_cohorts){
  treated_roots <- unique(covars_match[g==g0&treat==1,cnpj_raiz])
  treated_mun <- unique(covars_match[g==g0&cnpj_raiz%chin%treated_roots,municipio_principal])
  d <- unique(copy(covars_match[g==g0 &
                                (cnpj_raiz%chin%treated_roots | (treat==0 & municipio_principal%in%treated_mun))]),
              by="cnpj_raiz")
  d[,treat:=as.integer(cnpj_raiz%chin%treated_roots)]
  d<-d[complete.cases(d[,c("municipio_principal",matching_covars),with=FALSE])]
  grid<-CJ(distance=c("glm","mahalanobis"),replace=FALSE,
           m.order=c("largest","farthest","random"),seed=1:12)
  candidates<-lapply(seq_len(nrow(grid)),function(i){
    z<-grid[i];set.seed(3854035+z$seed)
    a<-list(reformulate(matching_covars,"treat"),data=d,method="nearest",distance=z$distance,
            ratio=1,replace=z$replace,m.order=z$m.order)
    fit<-tryCatch(do.call(matchit,a),error=function(e)NULL)
    if(is.null(fit))return(NULL)
    m<-as.data.table(match.data(fit,drop.unmatched=TRUE))
    coverage<-uniqueN(m[treat==1,cnpj_raiz])/length(treated_roots)
    imbalance<-max(abs(vapply(matching_covars,function(v)smd(m,v),numeric(1))))
    list(m=m,score=imbalance+10*(1-coverage),imbalance=imbalance,coverage=coverage)
  })
  candidates<-Filter(Negate(is.null),candidates)
  best<-candidates[[which.min(vapply(candidates,`[[`,numeric(1),"score"))]]
  m<-best$m
  m[,`:=`(coorte_match=g0,exact_key=paste(municipio_principal,broad_sector,
                                           tamanho_estab_principal,sep="|"))]
  matched_parts[[as.character(g0)]]<-m[,.(coorte_match,cnpj_raiz,treat,weights,exact_key,
                                          municipio_principal,broad_sector,tamanho_estab_principal,
                                          prop_homens,prop_pretos)]
  message(sprintf("Coorte %d: cobertura %.1f%%; maior SMD = %.3f",g0,100*best$coverage,best$imbalance))
}
new_units<-rbindlist(matched_parts,fill=TRUE)
fwrite(new_units,units_file,sep=";",bom=TRUE)
units <- new_units[,.(treat=max(treat),match_weight=sum(weights)),by=.(coorte_match,cnpj_raiz)]
units[,unit_id:=paste(coorte_match,cnpj_raiz,sep="_")]

# Reconstrói os outcomes diretamente dos microdados de vínculos da RAIS.
# A leitura é restrita aos CNPJs selecionados pelo matching e fica em cache.
outcome_file <- file.path(data_dir,"produto3_outcomes_rais.csv")
# Partimos do universo completo de controles e preservamos uma lista ampla
# dos dois vizinhos mais próximos por tratada para as regras exatas de setor e
# de setor x porte. O painel RAIS é extraído para esses candidatos antes de
# aplicar sobrevivência/balanceamento e refazer o matching final 1:1.
candidate_roots <- list()
for(g0 in matching_cohorts){
  d0<-unique(copy(covars_match[g==g0]),by="cnpj_raiz")
  d0[,division:=substr(as.character(as.integer(cnae_classe_principal)),1,2)]
  d0<-d0[complete.cases(d0[,c("municipio_principal",matching_covars,"division","tamanho_estab_principal"),with=FALSE])]
  for(exact_formula in list(~division,~division+tamanho_estab_principal)){
    fit0<-matchit(reformulate(matching_covars,"treat"),data=d0,method="nearest",
                  distance="mahalanobis",exact=exact_formula,ratio=2,
                  replace=FALSE,m.order="farthest")
    candidate_roots[[length(candidate_roots)+1L]]<-
      as.data.table(match.data(fit0,drop.unmatched=TRUE))$cnpj_raiz
  }
}
selected_roots <- unique(c(units$cnpj_raiz,unlist(candidate_roots,use.names=FALSE)))
cached <- if(file.exists(outcome_file)) fread(outcome_file,sep=";",colClasses=c(cnpj_raiz="character")) else data.table()
if(nrow(cached))cached[,cnpj_raiz:=clean_root(cnpj_raiz)]
# A base intermediária anterior já contém as massas de remuneração média e de
# dezembro para 2021--2025. Reaproveitá-la evita reler dezenas de GB da RAIS
# apenas para calcular médias por vínculo. Para 2020, o salário médio pode ser
# recuperado da massa salarial já extraída; remuneração de dezembro fica NA.
legacy_panel_file<-file.path(data_dir,"produto3_painel_rais.csv")
if(nrow(cached)){
  if(!"log_salario_medio"%in%names(cached))
    cached[,log_salario_medio:=fifelse(emprego>0&massa_salarial>0,log(massa_salarial/emprego),NA_real_)]
  if(!"log_salario_dezembro"%in%names(cached))cached[,log_salario_dezembro:=NA_real_]
  if(file.exists(legacy_panel_file)){
    legacy_panel<-fread(legacy_panel_file,sep=";",colClasses=c(cnpj_raiz="character"))
    legacy_panel[,cnpj_raiz:=clean_root(cnpj_raiz)]
    legacy_panel[,`:=`(
      log_salario_medio_legacy=fifelse(total_empregados>0&massa_salarial_media>0,
                                      log(massa_salarial_media/total_empregados),NA_real_),
      log_salario_dezembro_legacy=fifelse(total_empregados>0&massa_salarial_dezembro>0,
                                         log(massa_salarial_dezembro/total_empregados),NA_real_))]
    cached[legacy_panel,on=.(cnpj_raiz,ano),`:=`(
      log_salario_medio=fcoalesce(i.log_salario_medio_legacy,log_salario_medio),
      log_salario_dezembro=i.log_salario_dezembro_legacy)]
  }
  fwrite(cached,outcome_file,sep=";",bom=TRUE)
}
coverage_file<-file.path(data_dir,"produto3_outcomes_rais_cobertura.csv")
covered<-if(file.exists(coverage_file))fread(coverage_file,sep=";",colClasses=c(cnpj_raiz="character"))$cnpj_raiz else character()
# Recupera o passe completo concluído antes da criação do marcador.
if(!length(covered)&&nrow(cached)&&all(2020:2025%in%cached$ano)&&
   min(cached[,uniqueN(cnpj_raiz),by=ano]$V1)>700){
  fwrite(data.table(cnpj_raiz=selected_roots),coverage_file,sep=";",bom=TRUE);covered<-selected_roots
}
cache_complete <- nrow(cached)>0 && all(c("log_emprego","log_massa_salarial","anos_estudo_medio","log_salario_hora",
                                          "log_salario_medio","log_salario_dezembro")%in%names(cached)) &&
  all(selected_roots%chin%covered)

norm_name<-function(x){x<-iconv(x,to="ASCII//TRANSLIT");toupper(gsub("[^A-Z0-9]+"," ",x))}
pick_name<-function(nms,patterns){z<-norm_name(nms);for(p in patterns){hit<-which(grepl(p,z));if(length(hit))return(nms[hit[1]])};stop("Coluna RAIS não encontrada: ",paste(patterns,collapse=" | "))}
num_br<-function(x){
  z<-as.character(x);comma<-grepl(",",z,fixed=TRUE)
  z[comma]<-gsub(",",".",gsub(".","",z[comma],fixed=TRUE),fixed=TRUE)
  suppressWarnings(as.numeric(z))
}
school_years<-function(x){map<-c(`1`=0,`2`=3,`3`=5,`4`=7,`5`=9,`6`=10,`7`=12,`8`=14,`9`=16,`10`=18,`11`=21);unname(map[as.character(suppressWarnings(as.integer(x)))])}

if(!cache_complete){
  rais_dir<-Sys.getenv("RAIS_DIR",unset="D:/rais")
  rg<-Sys.which("rg");if(!nzchar(rg))stop("ripgrep (rg) não encontrado para filtrar os microdados RAIS")
  roots_to_extract<-selected_roots
  years<-2020:2025
  annual<-list()
  for(yr in years){
    pattern_file<-tempfile(fileext=".txt")
    exact_patterns<-if(yr<=2022L)paste0(";",roots_to_extract,";")else paste0(",\"",roots_to_extract,"\",")
    writeLines(exact_patterns,pattern_file,useBytes=TRUE)
    folder<-file.path(rais_dir,paste0("RAIS_",yr))
    # Filtra a pasta anual em uma única chamada; o rg percorre os arquivos
    # regionais em paralelo. Cada ano é salvo como checkpoint.
    hits_year<-tempfile(fileext=".txt")
    status_year<-system2(rg,c("--no-line-number","--no-filename","--text","--fixed-strings",
                              "--file",shQuote(pattern_file),"--glob",shQuote("*VINC*"),
                              shQuote(folder)),stdout=hits_year,stderr=FALSE)
    if(status_year>1L)stop("Falha ao filtrar RAIS ",yr," (rg status ",status_year,")")
    dt_year<-if(file.exists(hits_year)&&file.info(hits_year)$size>0)
      fread(hits_year,header=FALSE,select=c(37L,3L,11L,23L,25L,28L),colClasses="character",
            encoding="Latin-1",showProgress=FALSE) else data.table()
    unlink(c(hits_year,pattern_file))
    if(nrow(dt_year)){
      setnames(dt_year,c("root","active","school","pay","pay_dec","hours"));dt_year[,root:=clean_root(root)]
      dt_year<-dt_year[root%chin%selected_roots & suppressWarnings(as.integer(active))==1L]
      dt_year[,`:=`(pay_n=num_br(pay),pay_dec_n=num_br(pay_dec),hours_n=num_br(hours),school_n=school_years(school))]
      annual[[as.character(yr)]]<-dt_year[,.(
        emprego=.N,massa_salarial=sum(pay_n[is.finite(pay_n)&pay_n>=0],na.rm=TRUE),
        salario_medio=if(any(is.finite(pay_n)&pay_n>0))mean(pay_n[is.finite(pay_n)&pay_n>0])else NA_real_,
        salario_dezembro=if(any(is.finite(pay_dec_n)&pay_dec_n>0))mean(pay_dec_n[is.finite(pay_dec_n)&pay_dec_n>0])else NA_real_,
        anos_estudo_medio=if(any(is.finite(school_n)))mean(school_n,na.rm=TRUE)else NA_real_,
        salario_hora=if(any(is.finite(pay_n)&pay_n>0&is.finite(hours_n)&hours_n>0))
          weighted.mean(pay_n/(hours_n*4.33),w=pmax(pay_n,0),na.rm=TRUE)else NA_real_
      ),by=.(cnpj_raiz=root)][,ano:=yr]
      annual[[as.character(yr)]][,`:=`(log_emprego=log1p(emprego),
        log_massa_salarial=log1p(massa_salarial),log_salario_hora=log(salario_hora),
        log_salario_medio=log(salario_medio),log_salario_dezembro=log(salario_dezembro))]
      checkpoint<-rbindlist(list(cached[ano!=yr],annual[[as.character(yr)]]),fill=TRUE)
      fwrite(checkpoint,outcome_file,sep=";",bom=TRUE)
      cached<-checkpoint
    }
    message(sprintf("RAIS %d concluída: %d CNPJs encontrados",yr,
                    if(length(annual[[as.character(yr)]]))uniqueN(annual[[as.character(yr)]]$cnpj_raiz)else 0L))
    next
    files<-list.files(folder,pattern="VINC.*\\.(txt|COMT)$",full.names=TRUE,ignore.case=TRUE)
    files<-files[!grepl("layout",files,ignore.case=TRUE)]
    pieces<-list()
    for(f in files){
      # O leiaute oficial mantém estas posições em 2020--2025:
      # ativo=3, escolaridade=11, remuneração média=23, remuneração de dezembro=25,
      # horas=28, CNPJ-raiz=37.
      hits<-tempfile(fileext=".txt")
      status<-system2(rg,c("--no-line-number","--no-filename","--text","--fixed-strings",
                           "--file",shQuote(pattern_file),shQuote(f)),stdout=hits,stderr=FALSE)
      if(status>1L)stop("Falha ao filtrar ",f," (rg status ",status,")")
      if(!file.exists(hits)||file.info(hits)$size==0){unlink(hits);next}
      dt<-fread(hits,header=FALSE,select=c(37L,3L,11L,23L,25L,28L),colClasses="character",
                encoding="Latin-1",showProgress=FALSE)
      unlink(hits)
      setnames(dt,c("root","active","school","pay","pay_dec","hours"))
      dt[,root:=clean_root(root)]
      dt<-dt[root%chin%selected_roots & suppressWarnings(as.integer(active))==1L]
      if(nrow(dt)){
        dt[,`:=`(pay_n=num_br(pay),pay_dec_n=num_br(pay_dec),hours_n=num_br(hours),school_n=school_years(school))]
        pieces[[length(pieces)+1L]]<-dt[,.(
          emprego=.N,
          massa_salarial=sum(pay_n[is.finite(pay_n)&pay_n>=0],na.rm=TRUE),
          salario_medio=if(any(is.finite(pay_n)&pay_n>0))mean(pay_n[is.finite(pay_n)&pay_n>0])else NA_real_,
          salario_dezembro=if(any(is.finite(pay_dec_n)&pay_dec_n>0))mean(pay_dec_n[is.finite(pay_dec_n)&pay_dec_n>0])else NA_real_,
          anos_estudo_medio=if(any(is.finite(school_n)))mean(school_n,na.rm=TRUE)else NA_real_,
          salario_hora=if(any(is.finite(pay_n)&pay_n>0&is.finite(hours_n)&hours_n>0))
            weighted.mean(pay_n/(hours_n*4.33),w=pmax(pay_n,0),na.rm=TRUE)else NA_real_
        ),by=root]
      }
      rm(dt);gc(FALSE)
    }
    unlink(pattern_file)
    if(length(pieces))annual[[as.character(yr)]]<-rbindlist(pieces)[,.(
      emprego=sum(emprego),massa_salarial=sum(massa_salarial),
      salario_medio=weighted.mean(salario_medio,emprego,na.rm=TRUE),
      salario_dezembro=weighted.mean(salario_dezembro,emprego,na.rm=TRUE),
      anos_estudo_medio=weighted.mean(anos_estudo_medio,emprego,na.rm=TRUE),
      salario_hora=weighted.mean(salario_hora,massa_salarial,na.rm=TRUE)
    ),by=.(cnpj_raiz=root)][,ano:=yr]
    message(sprintf("RAIS %d concluída: %d CNPJs encontrados",yr,if(length(pieces))uniqueN(annual[[as.character(yr)]]$cnpj_raiz)else 0L))
  }
  rebuilt<-rbindlist(annual,fill=TRUE)
  rebuilt[,`:=`(log_emprego=log1p(emprego),log_massa_salarial=log1p(massa_salarial),
                log_salario_hora=log(salario_hora),log_salario_medio=log(salario_medio),
                log_salario_dezembro=log(salario_dezembro))]
  outcomes<-rbindlist(list(cached[cnpj_raiz%chin%selected_roots],rebuilt),fill=TRUE)
  setorder(outcomes,cnpj_raiz,ano);outcomes<-unique(outcomes,by=c("cnpj_raiz","ano"),fromLast=TRUE)
  fwrite(outcomes,outcome_file,sep=";",bom=TRUE)
  fwrite(data.table(cnpj_raiz=selected_roots),coverage_file,sep=";",bom=TRUE)
}else outcomes<-cached
outcomes[,cnpj_raiz:=clean_root(cnpj_raiz)]
outcomes<-unique(outcomes[,.(cnpj_raiz,ano,anos_estudo_medio,log_emprego,log_massa_salarial,log_salario_hora,
                             log_salario_medio,log_salario_dezembro)],by=c("cnpj_raiz","ano"))
panel <- merge(outcomes,units,by="cnpj_raiz",allow.cartesian=TRUE)

specs <- copy(outcomes_produto3)
setnames(specs, "outcome", "variable")
specs[, file := paste0("Event study - CS - ", safe, ".png")]

make_plot <- function(outcome,file){
  # Um CNPJ entra uma única vez: coorte real para tratados e zero para os
  # controles nunca tratados. O matching já foi feito antes desta etapa.
  cs_units<-units[,.(g=if(any(treat==1L))as.numeric(unique(coorte_match[treat==1L])[1])else 0),by=cnpj_raiz]
  d<-merge(outcomes[,c("cnpj_raiz","ano",outcome),with=FALSE],cs_units,by="cnpj_raiz")
  setnames(d,outcome,"y");d<-d[is.finite(y)]
  d[,id_num:=as.integer(factor(cnpj_raiz))]
  fit<-att_gt(yname="y",tname="ano",idname="id_num",gname="g",data=as.data.frame(d),
              xformla=~1,control_group="nevertreated",base_period="universal",
              panel=TRUE,allow_unbalanced_panel=TRUE,est_method="dr",
              bstrap=FALSE,cband=FALSE,alp=.05,print_details=FALSE)
  es<-aggte(fit,type="dynamic",min_e=-4,max_e=1,na.rm=TRUE,
            bstrap=FALSE,cband=FALSE,alp=.05)
  # Intervalos de confiança pontuais de 95% para cada tempo relativo.
  crit<-qnorm(.975)
  dynamic<-data.table(event_time=as.integer(es$egt),att=as.numeric(es$att.egt),se=as.numeric(es$se.egt))
  # No período-base universal, o pacote did retorna ATT = 0 e pode devolver
  # EP = NA. O intervalo correto da categoria normalizada é [0, 0].
  dynamic[event_time == -1L, `:=`(att=0, se=0)]
  dynamic[,`:=`(low=att-crit*se,high=att+crit*se)]
  overall <- as.numeric(es$overall.att); overall_se <- as.numeric(es$overall.se)
  z <- abs(overall/overall_se);stars<-if(z>=2.576)"***" else if(z>=1.96)"**" else if(z>=1.645)"*" else ""
  box <- sprintf("ATT: %.3f%s\n(%.3f)",overall,stars,overall_se)
  x_span <- diff(range(dynamic$event_time)); y_span <- diff(range(c(dynamic$low,dynamic$high),na.rm=TRUE))
  label_x <- max(dynamic$event_time)-.03*x_span; label_y <- max(dynamic$high,na.rm=TRUE)-.04*y_span
  p <- ggplot(dynamic,aes(event_time,att))+geom_hline(yintercept=0,color="grey45",linewidth=.35)+
    geom_vline(xintercept=-1,color="grey60",linetype="dashed",linewidth=.35)+
    geom_errorbar(aes(ymin=low,ymax=high),width=.08,color=cor_azul,linewidth=.55)+
    geom_point(color=cor_azul,size=2.2)+
    scale_x_continuous(breaks=dynamic$event_time)+labs(x="Tempo relativo ao primeiro apoio FUST",y="ATT")+
    annotate("label",x=label_x,y=label_y,label=box,hjust=1,vjust=1,size=4.2,label.size=.25,fill="white")+
    tema_bndes(legend_position="none")
  ggsave(file.path(output_dir,file),p,width=8,height=5,dpi=300,bg="white")
}
for(i in seq_len(nrow(specs)))make_plot(specs$variable[i],specs$file[i])

# -------------------------------------------------------------------------
# Robustez: funil progressivo, sempre refazendo o matching 1:1.
# S1 é a especificação-base. S2--S5 usam candidatos escolhidos no universo
# completo, nunca apenas os 274 controles da S1.
# -------------------------------------------------------------------------
norm_text<-function(x){x<-iconv(as.character(x),to="ASCII//TRANSLIT");toupper(gsub("[^A-Z0-9]+","",x))}
wb_fust<-file.path(data_dir,"Base de Operações_FUST_com_num_OPE_subcrédito.xlsx")
bndes_parts<-lapply(1:2,function(s){
  z<-as.data.table(read_excel(wb_fust,sheet=s,skip=if(s==1)4 else 5))
  z[,.(key=norm_text(Cliente),subset_bndes=norm_text(`Subsetor BNDES`),porte_bndes=norm_text(`Porte do cliente`))]
})
bndes_key<-rbindlist(bndes_parts)[,.(
  subset_bndes=names(sort(table(subset_bndes),decreasing=TRUE))[1],
  porte_bndes=names(sort(table(porte_bndes),decreasing=TRUE))[1]),by=key]
modality<-fread(file.path(data_dir,"produto3_modalidade_fust.csv"),sep=";",colClasses=c(cnpj_raiz="character"))
modality[,cnpj_raiz:=clean_root(cnpj_raiz)]
bndes_root<-unique(merge(modality[grepl("^[0-9]{8}$",cnpj_raiz),.(cnpj_raiz,key)],bndes_key,by="key"),by="cnpj_raiz")

rob_cov<-unique(copy(covars_match[g%in%matching_cohorts]),by=c("g","cnpj_raiz"))
rob_cov[,division:=substr(as.character(as.integer(cnae_classe_principal)),1,2)]
rob_cov[,subset_rais:=fcase(division=="61","TELECOMUNICACOES",division=="26","MECANICA",
                            division=="42","CONSTRUCAO",default="COMERCIOESERVICOS")]
coherent_roots<-merge(rob_cov[treat==1,.(cnpj_raiz,subset_rais)],bndes_root,by="cnpj_raiz")[subset_rais==subset_bndes,unique(cnpj_raiz)]

obs_years<-unique(outcomes[,.(cnpj_raiz,ano)])
has_years<-function(ids,yrs)obs_years[cnpj_raiz%chin%ids & ano%in%yrs,.(n=uniqueN(ano)),by=cnpj_raiz][n==length(yrs),cnpj_raiz]
has_post<-function(ids,g0)has_years(ids,g0:2025)
has_window<-function(ids,g0)has_years(ids,if(g0==2024L)2020:2025 else 2021:2025)

match_robust<-function(stage){
  if(stage=="S1")return(copy(new_units))
  out<-list();used_controls<-character()
  for(g0 in matching_cohorts){
    d<-unique(copy(rob_cov[g==g0 & cnpj_raiz%chin%selected_roots]),by="cnpj_raiz")
    d<-d[complete.cases(d[,c(matching_covars,"division","tamanho_estab_principal"),with=FALSE])]
    d<-d[(treat==0)|(treat==1&cnpj_raiz%chin%coherent_roots)]
    if(stage%in%c("S3","S4","S5")){
      ok<-has_post(d$cnpj_raiz,g0);d<-d[cnpj_raiz%chin%ok]
    }
    if(stage%in%c("S4","S5")){
      ok<-has_window(d$cnpj_raiz,g0);d<-d[cnpj_raiz%chin%ok]
    }
    d<-d[!(treat==0&cnpj_raiz%chin%used_controls)]
    exact_f<-if(stage=="S5")~division+tamanho_estab_principal else ~division
    fit<-matchit(reformulate(matching_covars,"treat"),data=d,method="nearest",
                 distance="mahalanobis",exact=exact_f,ratio=1,replace=FALSE,m.order="farthest")
    m<-as.data.table(match.data(fit,drop.unmatched=TRUE));m[,coorte_match:=g0]
    used_controls<-union(used_controls,m[treat==0,cnpj_raiz])
    out[[as.character(g0)]]<-m
  }
  rbindlist(out,fill=TRUE)
}

stage_labels<-c(
  S1="Covariadas completas (base)",
  S2="S1 + setor BNDES/RAIS coerente e setor exato",
  S3="S2 + presença após o tratamento",
  S4="S3 + painel balanceado na janela",
  S5="S4 + porte RAIS exato")
rob_matches<-setNames(lapply(names(stage_labels),match_robust),names(stage_labels))

# A especificação S4 passa a ser a amostra oficial do produto 3.
official_match<-copy(rob_matches[["S4"]])
fwrite(official_match,units_file,sep=";",bom=TRUE)
units<-official_match[,.(treat=max(treat),match_weight=sum(weights)),by=.(coorte_match,cnpj_raiz)]
units[,unit_id:=paste(coorte_match,cnpj_raiz,sep="_")]
# Sobrescreve as três figuras principais com a S4 e IC pontual de 95%.
for(i in seq_len(nrow(specs)))make_plot(specs$variable[i],specs$file[i])
cat("Event studies principais concluídos para emprego, massa salarial e salário médio.\n")
