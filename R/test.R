# Exemplo para gastos ----------------------------------------------------------
vest_gastos(url_portal = "http://www.gestaodinheiropublico.pr.gov.br/Gestao/",
            url_gastos = "portaldatransparencia/lei131/cons_despesa_orgao.jsp?",
            mes = "1",
            ano = "2016",
            natdespesa = "99999999",
            vdespesa = "E",
            orgao = sort(listar_orgaos(tipo = "despesa"))[1],
            vrecursos = "G",
            curl = T)

# Exemplo para gastos ----------------------------------------------------------
vest_receita_orgao(
  url_portal = "http://www.gestaodinheiropublico.pr.gov.br/Gestao/",
            url_receitas = "portaldatransparencia/lei131/cons_receita_orgao.jsp?",
            mes = "1",
            ano = "2016",
            receita = "999999999",
            vadmin = "G",
            orgao = sort(listar_orgaos(tipo = "receita"))[1],
            vrecursos = "G",
            curl = T)

# Looping Receita ---------------
looping_receita <- function(meses = 1:12, anos = 2003:2016,
                            orgao = sort(listar_orgaos(tipo = "receita"))[1]){
meses <- as.character(meses)
anos <- as.character(anos)

x <- as.list(rep(NA,length(anos)))
for(i in 1:length(anos))
  x[[i]] <- as.list(rep(NA,length(meses)))

for(i in 1:length(anos)){
  for(j in 1:length(meses)){
    x[[i]][[j]] <- vest_receita_orgao(url_portal = portal,
                                      url_receitas = "portaldatransparencia/lei131/cons_receita_orgao.jsp?",
                                      orgao = orgao,
                                      ano = anos[i], mes = meses[j],
                                      curl = TRUE)
    cat("ano", anos[i], "mes", j,  "\n")
  }
}

dados <- sapply(rep(NA,length(anos)),function(x) NULL)
for(i in 1:length(anos) ) {
  dados[[i]] <- rbindlist(x[[i]])
}
rbindlist(dados)
}
dados<-looping_receita(meses = 1:12, anos = 2003:2016, sort(listar_orgaos(tipo = "receita"))[1])

# --- Plot

library(ggplot2)
library(lubridate)
g<-dados[Código=="1000.0000",.(Receita = `No Mês`, Data = ymd(paste(Ano,Mês,"01"))), Mês ]
ggplot(g, aes(Data,Receita) ) +
  geom_line()
facet_grid(Mês ~ .)
