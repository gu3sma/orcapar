library(rvest)
library(data.table)


read_html("http://www.portaldatransparencia.pr.gov.br/modules/conteudo/conteudo.php?conteudo=68") %>%
  html_node("#combobox")

# Definicoes do atributos no portal de transparencia -----------------------------------------
# Gastos
portal     <- "http://www.gestaodinheiropublico.pr.gov.br/Gestao/"
gastos     <- "portaldatransparencia/lei131/cons_despesa_orgao.jsp?"

mes.        <- as.character("1")
ano.        <- as.character("2016")
natdespesa. <- as.character("99999999")
vdespesa.   <- as.character("E")
orgao.      <- as.character("0500%20-%20TRIBUNAL%20DE%20JUSTI%C7A")
vrecursos.  <- as.character("G")


# Definicoes do atributos no portal de transparencia -----------------------------------------
# Receitas


# Vest Portal Gasto --------------------
transp_vest_gastos <- function(url_portal = portal, url_gastos = gastos, mes = mes.,
                       ano = ano., natdespesa = natdespesa., vdespesa = vdespesa.,
                       orgao = orgao., vrecursos = vrecursos.){

url_vest <- paste(portal, gastos, "mes=", mes, "&ano=", ano,
                  "&natdespesa=", natdespesa, "&vdespesa=", vdespesa,
                  "&orgao=", orgao, "&vrecursos=", vrecursos,
                  sep="")

read_html(url_vest) %>%
  html_node(".tabela") %>%
  html_table(header = F, fill = T, dec = ".") %>%
  data.table() %>%
  setnames(as.character(.[2, ])) %>%
  .[4:nrow(.) - 1, ] %>%
  data.table() %>%
  .[, ':=' (Orgão = orgao, Ano = ano, Mês = mes)] %>%
  .[,`Dotação Inicial`:=(gsub(".","",`Dotação Inicial`,fixed = TRUE))] %>%
  .[,`Dotação Inicial`:=(as.numeric(gsub(",",".",`Dotação Inicial`,fixed = TRUE)))] %>%
  .[,`Autorizado`:=(gsub(".","",`Autorizado`,fixed = TRUE))]  %>%
  .[,`Autorizado`:=(as.numeric(gsub(",",".",`Autorizado`,fixed = TRUE)))] %>%
  .[,`No Mês`:=(gsub(".","",`No Mês`,fixed = TRUE))] %>%
  .[,`No Mês`:=(as.numeric(gsub(",",".",`No Mês`,fixed = TRUE)))] %>%
  .[,`Até Mês`:=(gsub(".","",`Até Mês`,fixed = TRUE))] %>%
  .[,`Até Mês`:=(as.numeric(gsub(",",".",`Até Mês`,fixed = TRUE)))] %>%
  data.table()
}

# Teste
transp_vest_gastos(url_portal = portal)
# - Testando no TJ
meses <- as.character(c(1:2))       # --- meses
anos <- as.character(c(2015:2016))   # --- anos

# Looping ---------------
rm(x)
x <- as.list(rep(NA,length(anos)))
for(i in 1:length(anos))
  x[[i]] <- as.list(rep(NA,length(meses)))


for(i in 1:length(anos)){
  for(j in 1:length(meses)){
    x[[i]][[j]] <- transp_vest_gastos(url_portal = portal, ano = anos[i], mes = meses[j])
    cat("ano", anos[i], "mes", j,  "\n")
    }
}

TJ <- data.frame(matrix((rep(NA,ncol(x[[1]][[1]]))),ncol=9))
for(i in 1:length(anos) ) {
TJ <- rbind(rbindlist(x[[i]]), TJ)}
TJ


TJ<-
  rbind(
  rbindlist(x[[1]]),
  rbindlist(x[[2]]),
  rbindlist(x[[3]]),
  rbindlist(x[[4]]),
  rbindlist(x[[5]]),
  rbindlist(x[[6]]),
  rbindlist(x[[7]]),
  rbindlist(x[[8]]),
  rbindlist(x[[9]]),
  rbindlist(x[[10]]),
  rbindlist(x[[11]]),
  rbindlist(x[[12]]),
  rbindlist(x[[13]])
  )

TJ
setkey(TJ, Código, Orgão, Ano, Mês)
key(TJ)
#
# (transp_vest(url_portal = portal, ano = "2016", mes = "01"),
   transp_vest(url_portal = portal, ano = "2016", mes = "02")
#   transp_vest(url_portal = portal, ano = "2016", mes = "03"))
#   ))

TJ[Código=="3000.0000",]
TJ[Código=="3000.0000" | Código=="Total",][1:100]

TJ[Código=="3100.0000",]
TJ

### RCL -----------------

# RECEITAS CORRENTES (I) -----------------
# 1100.0000	RECEITA TRIBUTÁRIA
#   1113.0200 ICMS
#   1112.0500 IPVA
#   1112.0700 ITCD
#   1112.0400 IRPF
#   XXXX.XXXX OUTRAS RECEIRAS TRIBUTÁRIAS = 1100.0000 RECEITA TRIBUTÁRIA - (ICMS  + IPVA + ITCD + IRPF)

# XXXX.XXXX Receita de Contribuições
# XXXX.XXXX Receita Patrimonial
# 1400.0000	RECEITA AGROPECUARIA
# 1500.0000	RECEITA INDUSTRIAL
# 1600.0000	RECEITA DE SERVICOS
# 1700.0000	TRANSFERENCIAS CORRENTES
#   1721.0101	Cota-Parte do FPE
#   XXXX.XXXX Transferências da LC 87/1996
#   1721.0112	C-PARTE IPI - PARC.ESTAD.
#   1724.0100	Transf. do FUNDEB
#   XXXX.XXXX Outras Transferências Correntes = (Diferença entre 1700.000 e os subelementos)
# 1900.0000	OUTRAS RECEITAS CORRENTES

## DEDUÇÕES (II) -------------
# 3340.8100 Transferências Constitucionais e Legais
# XXXX.XXXX Contrib. do Servidor para o Plano de Previdência
# XXXX.XXXX Compensação Financ. entre Regimes Previdência
# 9000.0000	DEDUCOES PARA O FUNDEB
