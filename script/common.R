require(tidyverse)
require(gt)

d <- data.frame(
  icon_num = factor(1:4, levels = 1:5),
  xbox = c("X", "Y", "A", "B"),
  key = c("T", "Y", "G", "H"),
  ps = c("□",  "△", "❌",  "○"),
  x = c(0, 1, 0, 1),
  y = c(1, 1, 0, 0)
  #x = c(0, 0.5, 0.5, 1),
  #y = c(0, sqrt(2)/2, -sqrt(2)/2, 0)
)
d_direction <- tibble(
  icon_num = factor(1:4, levels = 1:5),
  xbox = c("U", "D", "L", "R"),
  key = c("U", "D", "L", "R"),
  x = c(.5, .5, 0, 1),
  y = c(1, 0, .5, .5)
)

ragg_png_mod <- function(...){
  ragg::agg_png(..., units = "in", res = 300)
}

ragg_png_mod <- function(...){
  ragg::agg_png(..., units = "in", res = 300)
}

make_buttons <- function(buttons, type = "xbox", dev = ragg_png_mod, save = T, dir = NULL, name = paste0(paste(buttons, collapse = "+"), ".png")){
  g <- d %>% mutate(!!sym(type) := if_else(key %in% buttons, !!sym(type), NA_character_)) %>%
    ggplot(aes(x = x, y = y)) +
    geom_point(
      aes(color = "black", fill = factor(if_else(!is.na(!!sym(type)), as.integer(icon_num), as.integer(5)), 1:5)),
      size = 15 * .pt, shape = 21, color = "black") +
    geom_text(aes(label = !!sym(type)), size = 10 * .pt) +
    coord_fixed(xlim = c(0 -1/.pt, 1 + 1/.pt), ylim = c(0, 1)) +
    scale_fill_manual(guide = F, breaks = factor(1:5), values = c("blue", "yellow", "green", "red", "grey")) +
    theme_void()
  if(save){
    ggsave(plot = g, device = dev, path = dir, filename = name)
  } else {
    return(g)
  }
}

make_direct_buttons <- function(
    buttons, dev = svglite::svglite, save = T, dir = NULL,
    name = paste0(paste(buttons, collapse = "+"), ".svg")
){
  dat <- d_direction %>% mutate(col := key %in% buttons)
  g <- ggplot() +
    aes(xend = x, x = 0.5, yend = y, y = .5, color = col) +
    geom_segment(arrow = arrow(angle = 45, length = unit(5 /.pt, units = "mm"), type = "closed"),
                 lineend = "butt", size =  4 /.pt, linejoin = "mitre", data = filter(dat, !col)) +
    geom_segment(arrow = arrow(angle = 45, length = unit(5 /.pt, units = "mm"), type = "closed"),
                 lineend = "butt", size =  4 /.pt, linejoin = "mitre", data = filter(dat, col)) +
    scale_color_manual(guide = F, breaks = c(T, F), values = c("black", "grey")) +
    coord_fixed(xlim = c(0 -1/.pt, 1 + 1/.pt), ylim = c(0 - 1/.pt, 1 + 1/.pt)) +
    theme_void()
  if(save){
    ggsave(plot = g, device = dev, path = dir, filename = name, width = 230/ (.pt * 200), height = 230 / (200 * .pt), standalone = F)
  } else {
    return(g)
  }
}

reuse_svg <- function(id){
  sprintf('<img height=40 width=40 href="%s" />', id)
}

replace_keys <- function(text){
  text %>% str_remove_all("\\s") %>%
    str_match_all("\\[([TYGHRLUD\\+]+?)\\]|[^\\[([TYGHRLUD\\+]+?)\\]]+") %>%
    map(~tibble(str = if(length(.x[, 1]) == 0) "" else .x[, 1]) %>% mutate(
      is_key = str_detect(str, "\\[([TYGHRLUD\\+]+?)\\]"),
      str = if_else(is_key,
                    str_replace_all(str, "\\[([TYGHUDLR\\+]+?)\\]", "/img/\\1.svg") %>%
                      # str_replace_all(str, "\\[([TYGHUDLR\\+]+?)\\]", "key-\\1") %>%
                      sapply(reuse_svg) %>%
                      unlist,
                    str),
    )
    ) %>% map_chr(~paste(.x$str, collapse = ", ") %>% str_remove("^,\\s"))
}

to_table <- function(d, lr = F){
  t <- d %>% mutate(
    name = paste(name_orig, paste0("<br>(", name, ")")),
    mode = factor(mode, levels = c("normal", "long", "low", "high", "iron", "donna", "boartusk", "holy", "roof", "fool", "misc"))
  ) %>%
    dplyr::select(-name_orig)
  if(lr){
    t <- t %>% mutate(
      common = is.na(lr),
      lr = if_else(common, "l", lr)) %>%
      pivot_wider(id_cols = c(mode, name, common), names_from = lr, values_from = command) %>%
      mutate(l = if_else(is.na(l), "x", l), r = if_else(common, "", if_else(is.na(r), "x", r))) %>%
      dplyr::select(-common) %>% mutate(across(c(l, r), replace_keys))
  } else{
    t <- dplyr::select(t, -one_of("lr")) %>% mutate(command = replace_keys(command))
  }
  t <- t %>% arrange(mode) %>% group_by(mode) %>% gt()
  # HTMLをエスケープする方法として tab_row_group を使うものがあるが, rownames が表示されたままになる
  # 存在しないグループを指定するとエラー
  # よって内部データを改ざんするしかない
  g_headings <- list(
    normal = "通常時",
    long = "ロングガード時 - [スペース] 押しっぱなし",
    low = html(
      sprintf("下段の構え時 - %s + [スペース] 押しっぱなし)",
              paste(reuse_svg("key-D"), collapse = "\n"))
    ),
    iron = html(
      sprintf("『鉄の門』の構え時 - %s + [スペース] 押しっぱなし",
              paste(readLines(here::here("img/D.svg")), collapse = "\n"))
    ),
    high = html(
      sprintf("『上段』の構え時 (%s + [スペース] 押しっぱなし)",
              paste(reuse_svg("key-U"), collapse = "\n"))
    ),
    boartusk = html(
      sprintf("『猪の牙』の構え時 (%s + 「スペース」押しっぱなし)",
              paste(reuse_svg("key-U"), collapse = "\n"))
    ),
    donna = html(
      sprintf("『貴婦人』の構え時 (%s  %s %s)",
              paste(reuse_svg("key-D"), collapse = "\n"),
              paste(reuse_svg("key-L"), collapse = "\n"),
              paste(reuse_svg("key-Y"), collapse = "\n")
      )
    ),
    holy = html(sprintf("『聖なる』構え時 (%s)", paste(reuse_svg("key-D"), reuse_svg("key-R"), reuse_svg("key-Y"), collapse = "\n"))),
    roof = html(sprintf("『屋根』の構え時 (%s + [スペース] 押しっぱなし)", reuse_svg("key-U"))),
    fool = html(sprintf("『愚者』の構え時 (%s + [スペース] 押しっぱなし)", reuse_svg("key-D"))),
    misc = html("共通コマンド")
  )
  t$`_stub_df`[["group_label"]] <- map(t$`_stub_df`[["group_label"]], ~html(g_headings[[.x]]))
  if(lr){
    t <- t  %>%
      cols_label(name = "名称", l = "左足が前", r = "右足が前") %>%
      tab_spanner("コマンド", columns = c("l", "r")) %>%
      fmt_markdown(c("l", "r"))
  } else {
    t <- t %>%
      fmt_markdown(columns = "command") %>%
      cols_label(name = "名称", command = "コマンド")
  }
  t %>% fmt_markdown(columns = "name") %>% fmt_markdown(columns = "mode")
}

# gt から不要なHTMLタグを削除する
as_html_table <- function(gt){
  x <- as_raw_html(gt, inline_css = F) %>% xml2::read_html()
  xml2::xml_remove(xml2::xml_find_all(x, "//style"))
  xml2::xml_child(xml2::xml_child(x)) %>% paste(collapse = "") %>% knitr::raw_html()
  xml2::xml_child(xml2::xml_child(x)) %>% paste(collapse = "") %>% knitr::raw_html()
}

print_pinterest_embed <- function(id){
  sprintf(
    '<iframe src="https://assets.pinterest.com/ext/embed.html?id=%s" frameborder="0" scrolling="no" ></iframe>',
    id
    )
}
