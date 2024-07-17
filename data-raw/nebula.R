devtools::load_all()

# download ----------------

purrr::walk(
  1965:2023, 
  nebula_download_year,
  path = "data-raw/html_ano",
  .progress = TRUE
)

# parse -------------------

arquivos <- fs::dir_ls("data-raw/html_ano")

arq <- arquivos[41]

extrai_infos_ano(arq)

nebula <- purrr::map(arquivos, extrai_infos_ano) |> 
  purrr::list_rbind(names_to = "arquivo") |> 
  dplyr::mutate(
    ano = fs::path_ext_remove(basename(arquivo)),
    .before = arquivo
  )

usethis::use_data(nebula, overwrite = TRUE)

nebula_tidy <- nebula |> 
  dplyr::select(-link) |> 
  dplyr::group_by(ano, categoria, id_nominee, tipo) |> 
  dplyr::summarise(
    txt = paste(txt, collapse = ", "), 
    winner = any(winner),
    .groups = "drop"
  ) |> 
  dplyr::filter(!is.na(tipo)) |> 
  dplyr::mutate(
    tipo = stringr::str_replace_all(tipo, "-", "_")
  ) |> 
  tidyr::pivot_wider(
    names_from = tipo,
    values_from = txt
  )

usethis::use_data(nebula_tidy, overwrite = TRUE)


View(nebula_tidy)
