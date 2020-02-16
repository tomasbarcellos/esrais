#' Gerar dados da RAIS do ES
#'
#' @return TRUE/FALSE dizendo se os arquivos foram criados ou não
#' @importFrom magrittr %>%
#'
#' @examples
#' # Essa função não deve ser rodada. É apenas a documentação
#' # do processo de gerar os dados do pacote.
gerar_dados <- function() {
  es <- readr::read_csv2("inst/extdata/ES2017.txt",
                         locale = readr::locale(encoding = "latin1"),
                         col_types = readr::cols_only(
                           "CNAE 2.0 Classe" = "c", "CNAE 2.0 Subclasse" = "c",
                           "CBO Ocupação 2002" = "d", "Qtd Hora Contr" = "d",
                           "Idade" = "d", "Município" = "d", "Raça Cor" = "c",
                           "Sexo Trabalhador" = "c", "Tamanho Estabelecimento" = "d",
                           "Vl Rem Janeiro CC" = "d", "Vl Rem Fevereiro CC" = "d",
                           "Vl Rem Março CC" = "d", "Vl Rem Abril CC" = "d",
                           "Vl Rem Maio CC" = "d", "Vl Rem Junho CC" = "d",
                           "Vl Rem Julho CC" = "d", "Vl Rem Agosto CC" = "d",
                           "Vl Rem Setembro CC" = "d", "Vl Rem Outubro CC" = "d",
                           "Vl Rem Novembro CC" = "d",  "Vl Remun Dezembro Nom" = "d"
                         )
  )

  salarios <- es %>%
    dplyr::mutate(id = row_number()) %>%
    dplyr::select(id, `Vl Rem Janeiro CC`:`Vl Remun Dezembro Nom`) %>%
    tidyr::gather(mes, salario, `Vl Rem Janeiro CC`:`Vl Remun Dezembro Nom`) %>%
    dplyr::mutate(mes = stringr::str_extract(mes, "(?<=Rem )\\w+|(?<=Rem )\\w+"),
           salario = as.numeric(salario))

  es_wide <- es %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    dplyr::select(id, `CNAE 2.0 Classe`:`Tamanho Estabelecimento`)

  estab <- es_wide %>%
    dplyr::mutate(
      tam_medio = factor(
        `Tamanho Estabelecimento`,
        labels = c(0, 2, 7, 15, 35, 75,175, 375, 750, 1100)
      ),
      tam_medio = as.numeric(as.character(tam_medio))
    ) %>%
    dplyr::count(tam_medio, `Tamanho Estabelecimento`, sort = TRUE) %>%
    dplyr::mutate(n_grupo = round(n / tam_medio))

  set.seed(123)

  estab_id <- estab %>%
    dplyr::mutate(
      n_grupo = ifelse(is.infinite(n_grupo), 0, n_grupo),
      id_emp = purrr::map2(`Tamanho Estabelecimento`, n_grupo,
                    ~paste0(formatC(.x, width = 2, flag = 0),
                            formatC(seq_len(.y), width = 6, flag = 0))),
      empresa = purrr::map2(id_emp, n, ~sample(.x, .y, replace = TRUE))
    )

  vinculos <- es_wide %>%
    dplyr::mutate(empresa = purrr::flatten_chr(estab_id$empresa))

  save(vinculos, file = "inst/extdata/vinculos.rda")
  save(salarios, file = "inst/extdata/salarios_es.rda")

  file.exists("inst/extdata/vinculos.rda") & file.exists("inst/extdata/salarios_es.rda")
}
