# caged

rm(list=ls())     #limpa objetos no environment

library(tidyverse)
library(janitor)
library(writexl)
library(utils)



# abre base ---------------------------------------------------------------

caminho <- "D:/Dados/Bases de dados/novo caged/atualizacao/"


caged_2021 <- read.csv2(paste0(caminho, "caged_2021.csv")) %>%
        select(c("competenciamov",
                 "saldomovimentacao",
                 "uf",
                 "subclasse",
                 "indtrabintermitente",
                 "indicadordeexclusao"))

caged_2021 <- caged_2021 %>%
        filter(indtrabintermitente==1) %>%
        filter(competenciamov>=202101) %>%
        select(-c("competenciamov", "indtrabintermitente"))



t1 <- caged_2021 %>%
        group_by(uf, subclasse) %>%

        summarise(estat_fdp = sum(saldomovimentacao[is.na(indicadordeexclusao)]),
                  exclusao = sum(saldomovimentacao[!is.na(indicadordeexclusao)])) %>%

        # tira exclusões dos desligamentos
        mutate(movimentacao = estat_fdp - exclusao,
               .keep = "unused") %>%

        rename("cnae" = subclasse)

t1

t_final <- left_join(rais, t1,
                     by = c("uf", "cnae")) %>%

        mutate(movimentacao = ifelse(is.na(movimentacao),
                                     0,
                                     movimentacao),

               estoque = n_intermitentes + movimentacao)





