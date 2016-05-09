library(rvest)
library(data.table)
library(magrittr)

# Vest Portal Gasto --------------------
vest_gastos <- function(url_portal = portal, url_gastos = gastos,
                               mes = mes., ano = ano., natdespesa = natdespesa.,
                               vdespesa = vdespesa., orgao = orgao.,
                               vrecursos = vrecursos., curl = TRUE){

url_vest <- paste(portal, gastos, "mes=", mes, "&ano=", ano,
                  "&natdespesa=", natdespesa, "&vdespesa=", vdespesa,
                  "&orgao=", if(curl==T) curlEscape(orgao) else orgao,
                  "&vrecursos=", vrecursos,
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

# Vest Portal Receita --------------------
vest_receita_orgao <- function(url_portal = portal, url_receitas = receitas,
                               mes = mes., ano = ano., receita = receitas.,
                               vadmin = vadmin., orgao = orgao.,
                               vrecursos = vrecursos., curl = TRUE){

  url_vest <-
  paste(url_portal, url_receitas, "mes=", mes, "&ano=", ano,
        "&receita=", receita, "&vadmin=", vadmin,
        "&orgao=", if(curl==T) curlEscape(orgao) else orgao,
        "&vrecursos=", vrecursos,
                    sep="")

  read_html(url_vest) %>%
    html_nodes("table") %>%
    extract2(4) %>%
    html_table(header = F, fill = T, dec = ".") %>%
    data.table() %>%
    setnames(as.character(.[3, ])) %>%
    .[5:nrow(.) - 1, .(Código, Descrição, `No Mês`, `Até Mês`)] %>%
    data.table() %>%
    .[, ':=' (Orgão = orgao, Ano = ano, Mês = mes)] %>%
    .[,`No Mês`:=(gsub(".","",`No Mês`,fixed = TRUE))] %>%
    .[,`No Mês`:=(as.numeric(gsub(",",".",`No Mês`,fixed = TRUE)))] %>%
    .[,`Até Mês`:=(gsub(".","",`Até Mês`,fixed = TRUE))] %>%
    .[,`Até Mês`:=(as.numeric(gsub(",",".",`Até Mês`,fixed = TRUE)))] %>%
    data.table()
  }

# Lista de orgãos para a receita -----------------------------------------------

listar_orgaos <- function(tipo = c("receita","despesa")){
  read_html(
    paste("http://www.gestaodinheiropublico.pr.gov.br/Gestao/",
          "lei131/form",tipo,".jsp",sep="")
  ) %>%
    html_nodes(xpath = '//*[@id="combobox"]') %>%
    html_nodes('option') %>%
    html_attr("value")
}

# Definicoes do atributos no portal de transparencia ---------------------------
# Gastos
portal     <- "http://www.gestaodinheiropublico.pr.gov.br/Gestao/"
gastos     <- "portaldatransparencia/lei131/cons_despesa_orgao.jsp?"

mes.        <- as.character("1")
ano.        <- as.character("2016")
natdespesa. <- as.character("99999999")
vdespesa.   <- as.character("E")
orgao.      <- listar_orgaos[1]
vrecursos.  <- as.character("G")


# Definicoes do atributos no portal de transparencia ---------------------------
# Receitas

portal     <- "http://www.gestaodinheiropublico.pr.gov.br/Gestao/"
receitas   <- "portaldatransparencia/lei131/cons_receita_orgao.jsp?" # - Receita por orgao
receitas   <- "lei131/cons_receita.jsp?"                             # - Receita total

mes.        <- as.character("1")
ano.        <- as.character("2016")
receitas. <- as.character("999999999")
vadmin.   <- as.character("G")
orgao.      <- listar_orgaos[1]
vrecursos.  <- as.character("G")
