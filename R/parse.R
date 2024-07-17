extrai_infos_li <- function(li) {

  links <- li |> 
    xml2::xml_find_all("./a") |> 
    xml2::xml_attr("href")
  nomes <- li |> 
    xml2::xml_find_all("./a") |> 
    xml2::xml_text()
  winner <- li |> 
    xml2::xml_find_first("./i[@class='fa fa-star']")
  is_winner <- length(winner) > 0
  
  titulos <- stringr::str_extract(
    links, "(?<=[=/])(nominee|nominated|publisher)"
  )

  tibble::tibble(
    tipo = titulos,
    txt = nomes,
    link = links
  ) |> 
    dplyr::mutate(winner = is_winner)
}

extrai_infos_ul <- function(ul) {
  ul |> 
    purrr::map(extrai_infos_li) |> 
    purrr::list_rbind(names_to = "id_nominee")
}



extrai_infos_ano <- function(arq) {
  div <- arq |> 
    xml2::read_html() |> 
    xml2::xml_find_first("//div[@class='awards_column']")

  titulos <- div |> 
    xml2::xml_find_all("./h4") |> 
    xml2::xml_text()

  listas <- div |> 
    xml2::xml_find_all("./ul") |> 
    purrr::map(\(x) xml2::xml_find_all(x, "./li"))
  
  result <- listas |> 
    purrr::map(extrai_infos_ul) |> 
    purrr::set_names(titulos) |> 
    purrr::list_rbind(names_to = "categoria")
  
  result
}