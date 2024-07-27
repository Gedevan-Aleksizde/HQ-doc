require(tidyverse)
require(gt)

MODE_LEVELS <- c("normal", "long", "low", "high", "iron", "donna", "boartusk", "holy", "roof", "fool", "misc")
BLANK_SYMBOL <- "-"

d <- data.frame(
  icon_num = factor(1:4, levels = 1:5),
  xbox = c("X", "Y", "A", "B"),
  key = c("T", "Y", "G", "H"),
  ps = c("□", 
         "△",
         "×",
         "○"
  ),
  x = c(-1, 1, -1, 1) / (300 * .pt),
  y = c(1, 1, -1, -1) / (300 * .pt)
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

make_buttons_svg <- function(
    buttons, type = "xbox",
    dev = svglite::svglite,
    save = T,
    dir = NULL,
    name = paste0(paste(buttons, collapse = "+"), ".svg"),
    standalone = F){
  if(type == "xbox"){
    g <- d %>% mutate(!!sym(type) := if_else(key %in% buttons, !!sym(type), NA_character_)) %>%
      ggplot(aes(x = x, y = y)) +
      geom_point(
        aes(fill = factor(if_else(!is.na(!!sym(type)), as.integer(icon_num), as.integer(5)), 1:5)),
        size = 20/ .pt, shape = 21, color = "black"
      ) +
      geom_text(
        aes(
          label = !!sym(type)
        ),
        size = 15 / .pt)
  } else {
    g <- d %>% mutate(
      !!sym(type) := if_else(key %in% buttons, !!sym(type), NA_character_),
      x = if(type == "key") if_else(y > 0, x - .15 * abs(x), x + .15 * abs(x)) else  x
    ) %>%
      ggplot(aes(x = x, y = y)) +
      geom_point(
        size = 20/ .pt, shape = ifelse(type == "key", 22, 21), color = "black",
        fill = "grey"
      ) +
      geom_text(
        aes(
          label = !!sym(type),
          color = factor(if_else(!is.na(!!sym(type)), as.integer(icon_num), as.integer(5)), 1:5)
        ),
        size = 15 / .pt)
  }
  g <- g +
    coord_fixed(xlim = c(-1, 1)/ (150 * .pt), ylim = c(-1, 1)/(150 * .pt)) +
    scale_fill_manual(guide = F, breaks = factor(1:5), values = c("blue", "yellow", "green", "orange", "grey")) +
    scale_color_manual(guide = F, breaks = factor(1:5), values = c("blue", "yellow", "green", "orange", "grey")) +
    theme_void(base_family = "Noto Sans CJK JP")
  if(save){
    ggsave(plot = g, device = dev, path = dir, filename = name, width = 1, height = 1, scale = .5, standalone = standalone)
  } else {
    return(g)
  }
}

make_direct_buttons <- function(
    buttons, dev = svglite::svglite, save = T, dir = NULL,
    name = paste0(paste(buttons, collapse = "+"), ".svg"),
    standalone = F
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
    ggsave(plot = g, device = dev, path = dir, filename = name, width = 230/ (.pt * 200), height = 230 / (200 * .pt), standalone = standalone)
  } else {
    return(g)
  }
}

reuse_svg <- function(id){
  sprintf("<img height=40 width=40 src='%s' />", id)
}

replace_keys <- function(text, dir = "../../../../img/xbox"){
  pat <- "\\[([TYGHRLUD]+?)\\]"
  text %>% str_replace_all("\\s+", " ") %>%
    str_replace_all(pat, reuse_svg(file.path(dir, "\\1.svg")))
}

to_table <- function(d, is_lr = F, dir = "../../../../img/xbox"){
  t <- d %>% mutate(
    name = paste(name_orig, paste0("<br>(", name, ")")),
    mode = factor(mode, levels = MODE_LEVELS)
  ) %>%
    dplyr::select(-name_orig)
  is_lr <- if(is.na(is_lr)) F else is_lr
  if(is_lr){
    t_bothsides <- t %>% filter(is.na(lr) | lr== "lr")
    t <- bind_rows(
      t %>% filter(!(is.na(lr) | lr== "lr")),
      t_bothsides %>% mutate(lr = "l"),
      t_bothsides %>% mutate(lr = "r")
    )
    t <- t %>% mutate(
      common = is.na(lr),
      lr = if_else(common, "l", lr)) %>%
      pivot_wider(id_cols = c(mode, name, common), names_from = lr, values_from = command) %>%
      mutate(
        l = if_else(is.na(l), BLANK_SYMBOL, l),
        r = if_else(common, "", if_else(is.na(r), BLANK_SYMBOL, r))
      ) %>%
      dplyr::select(-common) %>% mutate(across(c(l, r), replace_keys, dir = dir))
  } else{
    t <- dplyr::select(t, -one_of("lr")) %>% mutate(command = replace_keys(command, dir = dir))
  }
  t <- t %>% arrange(mode) %>% group_by(mode) %>% gt()
  # TODO: HTMLをエスケープする方法として tab_row_group を使うものがあるが, rownames が表示されたままになる
  # 一方で, 存在しないグループを指定するとエラーが発生する
  # よって, HTMLタグを挿入するには内部データを改ざんするしかなさそう
  g_headings <- list(
    normal = "通常時",
    long = "ロングガード時 - スペース or LT 押しっぱなし",
    low = html(
      sprintf("下段の構え時 - %s + スペース or LT 押しっぱなし)",
              paste(reuse_svg(file.path(dir, "D.svg")), collapse = "\n"))
    ),
    iron = html(
      sprintf("『鉄の門』の構え時 - %s + スペース or LT 押しっぱなし",
              paste(reuse_svg(file.path(dir, "D.svg")), collapse = "\n")
              )
    ),
    high = html(
      sprintf("『上段』の構え時 (%s + スペース or LT 押しっぱなし)",
              paste(reuse_svg(file.path(dir, "U.svg")), collapse = "\n"))
    ),
    boartusk = html(
      sprintf("『猪の牙』の構え時 (%s + スペース or LT 押しっぱなし)",
              paste(reuse_svg(file.path(dir, "U.svg")), collapse = "\n"))
    ),
    donna = html(
      sprintf("『貴婦人』の構え時 (%s  %s %s)",
              paste(reuse_svg(file.path(dir, "D.svg")), collapse = "\n"),
              paste(reuse_svg(file.path(dir, "L.svg")), collapse = "\n"),
              paste(reuse_svg(file.path(dir, "T.svg")), collapse = "\n")
      )
    ),
    holy = html(
      sprintf("『聖なる』構え時 (%s)",
              paste(
                reuse_svg(file.path(dir, "D.svg")),
                reuse_svg(file.path(dir, "R.svg")),
                reuse_svg(file.path(dir, "Y.svg")), collapse = "\n")
              )),
    roof = html(sprintf("『屋根』の構え時 (%s + スペース or LT 押しっぱなし)", reuse_svg(file.path(dir, "U.svg")))),
    fool = html(sprintf("『愚者』の構え時 (%s + スペース or LT 押しっぱなし)", reuse_svg(file.path(dir, "D.svg")))),
    misc = html("共通コマンド")
  )
  t$`_stub_df`[["group_label"]] <- map(t$`_stub_df`[["group_label"]], ~html(g_headings[[.x]]))
  if(is_lr){
    t <- t  %>%
      cols_label(name = "名称", l = "左足が前", r = "右足が前") %>%
      tab_spanner("コマンド", columns = c("l", "r")) %>%
      fmt_markdown(c("l", "r"))
  } else {
    t <- t %>%
      fmt_markdown(columns = "command") %>%
      cols_label(name = "名称", command = "コマンド")
  }
  t %>%
    tab_style(
      style = list(
        cell_text(align = "center", weight = "bold")
      ),
      locations = cells_stubhead()
    ) %>%
    fmt_markdown(columns = "name") %>% fmt_markdown(columns = "mode")
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
