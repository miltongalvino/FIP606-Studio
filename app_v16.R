library(shiny)
library(bslib)
library(tidyverse)
library(ggplot2)
library(plotly)
library(readxl)
library(DT)
library(rstatix)
library(emmeans)
library(lme4)
library(lmerTest)
library(multcompView)
library(multcomp)
library(epifitter)
library(ggpubr)
library(DHARMa)
library(shinycssloaders)
library(car)
library(patchwork)
library(systemfonts)
library(sf)
sf_use_s2(FALSE)
library(rnaturalearth)
library(rnaturalearthdata)
library(effectsize)
library(clipr)
# Configura a pasta de aulas para servir arquivos HTML estáticos
addResourcePath("aulas_html", "aulas")

# ─── Helpers ──────────────────────────────────────────────
clean_cld <- function(em_model) {
  letras <- cld(em_model, Letters = letters)
  letras$.group <- trimws(letras$.group)
  as.data.frame(letras)
}

generate_factorial_table <- function(m, trat1, trat2) {
  # minúsculas: compara trat1 dentro de cada trat2
  em_min <- emmeans(m, as.formula(paste("~", trat1, "|", trat2)))
  cld_min <- cld(em_min, Letters = letters)
  cld_min$.group <- trimws(cld_min$.group)
  
  # maiúsculas: compara trat2 dentro de cada trat1
  em_mai <- emmeans(m, as.formula(paste("~", trat2, "|", trat1)))
  cld_mai <- cld(em_mai, Letters = LETTERS)
  cld_mai$.group <- trimws(cld_mai$.group)
  
  # Junta os dois
  df_min <- as.data.frame(cld_min)
  df_mai <- as.data.frame(cld_mai)
  
  # Merge
  df_join <- merge(df_min, df_mai[, c(trat1, trat2, ".group")], by = c(trat1, trat2), suffixes = c("_min", "_mai"))
  
  # Formata "Mean ± SE aA"
  df_join$cell <- sprintf("%.2f &plusmn; %.2f %s%s", 
                          df_join$emmean, df_join$SE, 
                          df_join$.group_min, df_join$.group_mai)
  
  # Pivot para tabela de dupla entrada: trat1 nas linhas, trat2 nas colunas
  tab <- tidyr::pivot_wider(df_join[, c(trat1, trat2, "cell")], names_from = all_of(trat2), values_from = "cell")
  
  return(tab)
}

palette_fip <- c("#2C3E50","#18BC9C","#E74C3C","#3498DB","#F39C12","#9B59B6","#1ABC9C","#E67E22")

# ─── Funcao de interpretacao ─────────────────────────────
badge_sig <- function(pval) {
  if (is.na(pval)) return('<span class="badge bg-secondary">N/A</span>')
  if (pval <= 0.001) return('<span class="badge bg-danger">p ≤ 0.001 — Altamente significativo</span>')
  if (pval <= 0.01)  return(paste0('<span class="badge bg-warning text-dark">p = ', format(pval, digits=3), ' — Significativo</span>'))
  if (pval <= 0.05)  return(paste0('<span class="badge bg-info">p = ', format(pval, digits=3), ' — Significativo (5%)</span>'))
  paste0('<span class="badge bg-success">p = ', format(pval, digits=3), ' — Não significativo</span>')
}

# ─── Helper: Paletas de cores ────────────────────────────
get_palette_colors <- function(palette_name) {
  switch(palette_name,
    "FIP606 (Padrão)" = c("#2C3E50","#18BC9C","#E74C3C","#3498DB","#F39C12","#9B59B6","#1ABC9C","#E67E22"),
    "Viridis"   = c("#440154","#46327E","#365C8D","#277F8E","#1FA187","#4AC16D","#9FDA3A","#FDE725"),
    "Set1"      = c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#FFFF33","#A65628","#F781BF"),
    "Set2"      = c("#66C2A5","#FC8D62","#8DA0CB","#E78AC3","#A6D854","#FFD92F","#E5C494","#B3B3B3"),
    "Dark2"     = c("#1B9E77","#D95F02","#7570B3","#E7298A","#66A61E","#E6AB02","#A6761D","#666666"),
    "Pastel1"   = c("#FBB4AE","#B3CDE3","#CCEBC5","#DECBE4","#FED9A6","#FFFFCC","#E5D8BD","#FDDAEC"),
    "Oceano"    = c("#023E8A","#0077B6","#0096C7","#00B4D8","#48CAE4","#90E0EF","#ADE8F4","#CAF0F8"),
    "Tropical"  = c("#FF6B6B","#4ECDC4","#45B7D1","#96CEB4","#FFEAA7","#DDA0DD","#FF8C42","#98D8C8"),
    "Tons de Cinza" = c("#000000", "#333333", "#666666", "#999999", "#CCCCCC", "#1A1A1A", "#4D4D4D", "#808080"),
    "Preto e Branco" = c("#000000", "#FFFFFF", "#000000", "#FFFFFF", "#000000", "#FFFFFF", "#000000", "#FFFFFF"),
    c("#2C3E50","#18BC9C","#E74C3C","#3498DB","#F39C12","#9B59B6","#1ABC9C","#E67E22")
  )
}

# ─── Helper: Temas ggplot ────────────────────────────────
get_theme_func <- function(theme_name) {
  switch(theme_name,
    "Clássico"       = theme_classic,
    "Minimalista"    = theme_minimal,
    "Preto e Branco" = theme_bw,
    "Claro"          = theme_light,
    "Vazio"          = theme_void,
    theme_classic
  )
}

# ─── Helper: Transformacoes ──────────────────────────────
transf_choices <- c("Nenhuma", "Log (ln)", "Log (y+1)", "Log\u2081\u2080",
                    "Raiz quadrada", "\u221a(y+0.5)", "Arco-seno \u221ay",
                    "Box-Cox", "Inversa (1/y)")

apply_transform <- function(y, type) {
  tryCatch({
    result <- switch(type,
      "Nenhuma"       = y,
      "Log (ln)"      = log(y),
      "Log (y+1)"     = log(y + 1),
      "Log\u2081\u2080" = log10(y),
      "Raiz quadrada" = sqrt(y),
      "\u221a(y+0.5)" = sqrt(y + 0.5),
      "Arco-seno \u221ay" = asin(sqrt(y)),
      "Box-Cox" = {
        y_clean <- y[y > 0 & !is.na(y)]
        if (length(y_clean) < 3) return(y)
        m_tmp <- lm(y_clean ~ 1)
        bc <- MASS::boxcox(m_tmp, plotit = FALSE)
        lambda <- bc$x[which.max(bc$y)]
        if (abs(lambda) < 0.01) log(y) else (y^lambda - 1) / lambda
      },
      "Inversa (1/y)" = 1 / y,
      y
    )
    result
  }, error = function(e) y, warning = function(w) {
    suppressWarnings(tryCatch(switch(type,
      "Log (ln)"      = log(y),
      "Log (y+1)"     = log(y + 1),
      "Log\u2081\u2080" = log10(y),
      "Raiz quadrada" = sqrt(y),
      "\u221a(y+0.5)" = sqrt(y + 0.5),
      "Arco-seno \u221ay" = asin(sqrt(y)),
      "Inversa (1/y)" = 1 / y,
      y
    ), error = function(e2) y))
  })
}

transf_label <- function(var_name, type) {
  switch(type,
    "Nenhuma"       = var_name,
    "Log (ln)"      = paste0("log(", var_name, ")"),
    "Log (y+1)"     = paste0("log(", var_name, "+1)"),
    "Log\u2081\u2080" = paste0("log\u2081\u2080(", var_name, ")"),
    "Raiz quadrada" = paste0("\u221a(", var_name, ")"),
    "\u221a(y+0.5)" = paste0("\u221a(", var_name, "+0.5)"),
    "Arco-seno \u221ay" = paste0("asin(\u221a", var_name, ")"),
    "Box-Cox"       = paste0("BoxCox(", var_name, ")"),
    "Inversa (1/y)" = paste0("1/", var_name),
    var_name
  )
}

add_geom_type <- function(p, type, alpha_val) {
  pos <- position_dodge(width=0.75)
  if (type == "Violin") {
    p + geom_violin(alpha=alpha_val, trim=FALSE, position=pos)
  } else if (type == "Barras com Erro") {
    p + stat_summary(fun=mean, geom="bar", width=0.6, color="black", alpha=alpha_val, position=pos) +
        stat_summary(fun.data=mean_se, geom="errorbar", width=0.2, position=pos)
  } else if (type == "Pontos com Erro") {
    p + stat_summary(fun.data=mean_se, geom="errorbar", width=0.2, position=pos) +
        stat_summary(fun=mean, geom="point", shape=21, size=5, color="black", alpha=1, position=pos)
  } else if (type == "Raincloud") {
    if (requireNamespace("ggdist", quietly = TRUE)) {
      p + ggdist::stat_halfeye(
            adjust = .5, width = .6, .width = 0, justification = -.2, point_colour = NA, alpha=alpha_val
          ) +
          geom_boxplot(width = .15, outlier.color = NA, alpha=alpha_val, justification = .15)
    } else {
      p + geom_violin(alpha=alpha_val, trim=FALSE, position=pos)
    }
  } else {
    p + geom_boxplot(width=0.6, alpha=alpha_val, outlier.color=NA, position=pos)
  }
}

# ─── Helper: Labels customizados ─────────────────────────
apply_custom_labels <- function(p, input) {
  if (!is.null(input$custom_title) && nzchar(trimws(input$custom_title))) p <- p + labs(title = input$custom_title)
  if (!is.null(input$custom_xlab)  && nzchar(trimws(input$custom_xlab)))  p <- p + labs(x = input$custom_xlab)
  if (!is.null(input$custom_ylab)  && nzchar(trimws(input$custom_ylab)))  p <- p + labs(y = input$custom_ylab)
  if (!is.null(input$custom_legend_title) && nzchar(trimws(input$custom_legend_title))) p <- p + labs(color = input$custom_legend_title, fill = input$custom_legend_title, shape = input$custom_legend_title, group = input$custom_legend_title)
  
  ang <- as.numeric(input$custom_x_angle %||% 0)
  if (ang > 0) p <- p + theme(axis.text.x = element_text(angle = ang, hjust = 1))
  
  ymin <- input$custom_y_min; ymax <- input$custom_y_max
  if (is.numeric(ymin) && is.numeric(ymax) && !is.na(ymin) && !is.na(ymax)) {
    p <- p + coord_cartesian(ylim = c(ymin, ymax))
  } else if (is.numeric(ymin) && !is.na(ymin)) {
    p <- p + coord_cartesian(ylim = c(ymin, NA))
  } else if (is.numeric(ymax) && !is.na(ymax)) {
    p <- p + coord_cartesian(ylim = c(NA, ymax))
  }
  p
}

# ─── Helper: Níveis de Fatores Customizados ───────────────
apply_plot_levels <- function(df, col1=NULL, levels1=NULL, col2=NULL, levels2=NULL) {
  if (!is.null(col1) && col1 %in% names(df) && !is.null(levels1) && nzchar(trimws(levels1))) {
    new_lvls <- trimws(unlist(strsplit(levels1, ",")))
    if(length(new_lvls) > 0) {
      df[[col1]] <- as.factor(df[[col1]])
      if (length(new_lvls) == length(levels(df[[col1]]))) {
        levels(df[[col1]]) <- new_lvls
      }
    }
  }
  if (!is.null(col2) && col2 != "Nenhum" && col2 %in% names(df) && !is.null(levels2) && nzchar(trimws(levels2))) {
    new_lvls <- trimws(unlist(strsplit(levels2, ",")))
    if(length(new_lvls) > 0) {
      df[[col2]] <- as.factor(df[[col2]])
      if (length(new_lvls) == length(levels(df[[col2]]))) {
        levels(df[[col2]]) <- new_lvls
      }
    }
  }
  df
}

# ─── Helper: Non-parametric Letters ───────────────────────
get_nonpar_letters <- function(df, resp, trat) {
  pwt <- suppressWarnings(pairwise.wilcox.test(df[[resp]], df[[trat]], p.adjust.method="holm", exact=FALSE))
  p_mat <- pwt$p.value
  lvls <- union(rownames(p_mat), colnames(p_mat))
  full_mat <- matrix(1, length(lvls), length(lvls), dimnames=list(lvls, lvls))
  for(i in rownames(p_mat)) {
    for(j in colnames(p_mat)) {
      if(!is.na(p_mat[i,j])) {
        full_mat[i, j] <- p_mat[i,j]
        full_mat[j, i] <- p_mat[i,j]
      }
    }
  }
  let <- multcompView::multcompLetters(full_mat)$Letters
  data.frame(trat_name = names(let), .group = unname(let), stringsAsFactors = FALSE)
}



# ─── CSS ──────────────────────────────────────────────────
custom_css <- "
  /* RendR Style Aesthetics - Premium Dark Emerald & Indigo Theme */
  .bslib-page-navbar .navbar { border-bottom: 1px solid #E2E8F0 !important; background-color: #FFFFFF !important; padding-top: 0.6rem !important; padding-bottom: 0.6rem !important; box-shadow: 0 4px 6px -1px rgba(0,0,0,0.05); }
  .navbar-brand { color: #0F172A !important; font-weight: 800; font-size: 1.4rem; letter-spacing: -0.5px; }
  .navbar-nav .nav-link { color: #64748B !important; font-weight: 600; font-size: 0.95rem; }
  .navbar-nav .nav-link.active, .navbar-nav .nav-link:hover { color: #059669 !important; }
  .top-banner { background: linear-gradient(135deg, #059669 0%, #047857 100%); color: #FFFFFF; text-align: center; padding: 8px 15px; font-size: 0.9rem; font-weight: 600; margin-top: -16px; margin-left: calc(-50vw + 50%); width: 100vw; margin-bottom: 0; box-shadow: 0 2px 4px rgba(5,150,105,0.3); }
  body { background-color: #F8FAFC; }
  
  /* RendR Container */
  .rendr-container { max-width: 800px; margin: 0 auto; padding: 0.5rem 2rem 1.5rem 2rem; text-align: center; }
  .rendr-title { font-size: 2.5rem; font-weight: 800; color: #0F172A; margin-bottom: 0.8rem; letter-spacing: -1px; }
  .rendr-subtitle { font-size: 1.1rem; color: #475569; margin-bottom: 1.5rem; line-height: 1.6; }

  /* Dashed Box */
  .dashed-box { border: 2px dashed #CBD5E1; border-radius: 16px; padding: 2.5rem 2rem; background: #FFFFFF; transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1); cursor: pointer; position: relative; box-shadow: 0 1px 2px rgba(0,0,0,0.02); }
  .dashed-box:hover { border-color: #059669; background: #ECFDF5; box-shadow: 0 10px 15px -3px rgba(5,150,105,0.1); transform: translateY(-2px); }
  .dashed-box-title { font-size: 1.3rem; font-weight: 700; color: #1E293B; margin-bottom: 0.4rem; }
  .dashed-box-sub { font-size: 1rem; color: #64748B; margin-bottom: 1.2rem; }
  .file-badges { display: flex; justify-content: center; gap: 10px; margin-bottom: 1rem; }
  .file-badge { background: #F1F5F9; border: 1px solid #E2E8F0; border-radius: 16px; padding: 4px 14px; font-size: 0.8rem; font-weight: 700; color: #475569; }
  .file-limit { font-size: 0.85rem; color: #94A3B8; }
  
  /* File Input Override inside dashed-box */
  .dashed-box .shiny-input-container { position: absolute !important; top: 0; left: 0; width: 100%; height: 100%; opacity: 0; cursor: pointer; margin: 0 !important; z-index: 100; overflow: hidden; }
  .dashed-box .input-group, .dashed-box .input-group-btn, .dashed-box .btn-file { height: 100%; width: 100%; cursor: pointer; margin: 0 !important; padding: 0 !important; }
  .dashed-box .btn-file { opacity: 0; position: absolute; top: 0; left: 0; }
  .dashed-box input[type='text'] { display: none; }
  
  /* Sheet selector center */
  #ui_sheet_selector { text-align: center; margin-top: 15px; }
  #ui_sheet_selector .control-label { text-align: center !important; display: block; font-weight: 700; margin-bottom: 8px; color: #1E293B; }
  #ui_sheet_selector .shiny-input-container { margin: 0 auto !important; }

  /* RendR Accordion */
  .rendr-accordion { margin-top: 2.5rem; text-align: left; }
  .rendr-accordion .accordion-item { border: 1px solid #E2E8F0; border-radius: 12px; margin-bottom: 0.8rem; overflow: hidden; box-shadow: 0 4px 6px -1px rgba(0,0,0,0.05); }
  .rendr-accordion .accordion-button { background-color: #FFFFFF; color: #475569; font-weight: 600; font-size: 1rem; padding: 1.2rem 1.5rem; box-shadow: none !important; }
  .rendr-accordion .accordion-button:not(.collapsed) { color: #FFFFFF; background-color: #064E3B; border-bottom: 1px solid #064E3B; box-shadow: 0 4px 10px rgba(6,78,59,0.3) !important; }
  .rendr-accordion .accordion-body { color: #475569; font-size: 0.95rem; background-color: #FFFFFF; padding: 1.5rem; line-height: 1.6; }
  
  /* Retain old styles for other tabs */
  .card { border: 1px solid #E2E8F0; border-radius: 16px; box-shadow: 0 10px 15px -3px rgba(0,0,0,0.08); margin-bottom: 1.5rem; background: #FFFFFF; overflow: hidden; }
  .card-header { background: #064E3B; color: #FFFFFF !important; border-bottom: none; border-radius: 16px 16px 0 0 !important; font-weight: 700; padding: 1.2rem 1.25rem; font-size: 1.15rem; box-shadow: 0 4px 6px -1px rgba(0,0,0,0.1); }
  .card-header .nav-link { color: rgba(255, 255, 255, 0.75) !important; font-weight: 600; }
  .card-header .nav-link:hover { color: #FFFFFF !important; }
  .card-header .nav-link.active { color: #064E3B !important; background-color: #FFFFFF !important; font-weight: 700; }
  .btn-info { background: #064E3B; border: none; color: #fff; font-weight: 600; border-radius: 8px; padding: 0.5rem 1rem; transition: all 0.2s; box-shadow: 0 4px 6px rgba(6,78,59,0.2); }
  .btn-info:hover { background: #022C22; color: #fff; transform: translateY(-2px); box-shadow: 0 6px 12px rgba(2,44,34,0.3); }
  .module-about { background: #F8FAFC; border: 1px solid #E2E8F0; border-radius: 12px; padding: 1.2rem; margin: .8rem 0; box-shadow: inset 0 2px 4px rgba(0,0,0,0.02); }
  .module-about h5 { color: #064E3B; font-weight: 800; margin-bottom: 0.8rem; }
  .section-hint { background: #F0FDF4; border-left: 5px solid #064E3B; padding: 14px 18px; font-size: .95rem; color: #064E3B; border-radius: 0 8px 8px 0; font-weight: 600; box-shadow: 0 2px 4px rgba(0,0,0,0.04); }
  
  /* Smart Example Cards Minimal */
  .example-mini-card { border: 1px solid #E2E8F0; border-radius: 10px; padding: 12px; margin-bottom: 10px; transition: all 0.2s; cursor: pointer; background: #FFFFFF; }
  .example-mini-card:hover { border-color: #4F46E5; background: #EEF2FF; transform: translateX(4px); }
  .example-mini-card .ex-icon { font-size: 1.3rem; color: #4F46E5; margin-right: 12px; }
  .example-mini-card .ex-title { font-weight: 700; color: #1E293B; font-size: 0.95rem; }

  /* Summary Cards pós-upload */
  .upload-summary { display: flex; flex-wrap: wrap; gap: 10px; justify-content: center; margin: 18px 0 6px 0; }
  .summary-chip { background: #F1F5F9; border: 1px solid #E2E8F0; border-radius: 20px; padding: 6px 16px; font-size: 0.85rem; font-weight: 700; color: #475569; display: flex; align-items: center; gap: 8px; box-shadow: 0 1px 2px rgba(0,0,0,0.02); }
  .summary-chip .chip-icon { color: #4F46E5; }
  .summary-chip.chip-ok { background: #D1FAE5; border-color: #A7F3D0; color: #065F46; }
  .summary-chip.chip-warn { background: #FEF3C7; border-color: #FDE68A; color: #92400E; }
  
  /* Improve Warning Yellow Contrast */
  .text-warning { color: #D97706 !important; }
  .btn-outline-warning { color: #D97706 !important; border-color: #D97706 !important; }
  .btn-outline-warning:hover { background-color: #D97706 !important; color: #FFFFFF !important; }
"


# ═══════════════════════════════════════════════════════════
#  UI
# ═══════════════════════════════════════════════════════════
ui <- page_navbar(
  id = "main_nav",
  title = tags$span(tags$img(src = "app_icon.png?v=4", height = "38px", style = "margin-right: 8px; vertical-align: middle; margin-top: -4px;"), tags$strong(" FIP606 Studio")),
  theme = bs_theme(version=5, bg="#F8FAFC", fg="#1E293B", primary="#059669", secondary="#4F46E5",
                   base_font=font_google("Inter"), heading_font=font_google("Inter")),
  header = list(
    tags$head(tags$style(HTML(custom_css))),
    tags$script(HTML("
      $(document).on('shiny:inputchanged', function(event) {
        if (event.name === 'main_nav') {
          if (event.value === 'aba_inicio') {
            $('body').addClass('hide-sidebar');
          } else {
            $('body').removeClass('hide-sidebar');
          }
        }
      });
      $(function() {
        $('body').addClass('hide-sidebar');
      });
    ")),
    tags$style(HTML("
      body.hide-sidebar .bslib-sidebar-layout > .collapse-toggle,
      body.hide-sidebar .bslib-page-navbar .navbar .collapse-toggle,
      body.hide-sidebar .bslib-sidebar-toggle,
      body.hide-sidebar aside.bslib-sidebar { display: none !important; }
    "))
  ),
  
  sidebar = sidebar(id="global_sidebar", width=340, title=tags$span(icon("cogs")," Configurações Globais"), open="closed",
    textInput("report_title", "Título do Relatório:", "Relatório Estatístico FIP606"),
    textInput("report_author", "Autor:", "Automático"),
    hr(),
    checkboxGroupInput("report_sections", tags$span(icon("list-check"), " Seções do Relatório:"),
      choices = c(
        "Análise Exploratória (AED)" = "aed",
        "Teste T / Wilcoxon" = "ttest",
        "ANOVA" = "anova",
        "GLM" = "glm",
        "Correlação" = "cor",
        "Regressão" = "reg",
        "AUDPC / Epidemiologia" = "audpc",
        "Mapas" = "mapa"
      ),
      selected = character(0)
    ),
    radioButtons("report_format", tags$span(icon("file-export"), " Formato:"),
      choices = c("Word (.docx)" = "word", "HTML" = "html"), selected = "word", inline = TRUE
    ),
    downloadButton("dl_report", tags$span(icon("file-word")," Baixar Relatório Completo"), class="btn-warning w-100 mb-2"),
    hr(),
    accordion(open = FALSE, multiple = TRUE,
      accordion_panel(title=tags$span(icon("magnifying-glass-plus"), icon("palette"), " Aparência de gráficos"), value="aparencia",
        conditionalPanel(
          condition = "input.main_nav !== 'aba_reg' && input.main_nav !== 'aba_mapa' && input.main_nav !== 'aba_audpc'",
          selectInput("custom_plot_type", tags$b("Tipo de Gráfico Principal:"), choices=c("Boxplot", "Violin", "Raincloud", "Barras com Erro", "Pontos com Erro"), selected="Boxplot")
        ),
        checkboxInput("use_plotly", tags$b("Ativar Gráficos Interativos (Plotly)"), value=FALSE),
        checkboxInput("custom_jitter", tags$b("Mostrar Pontos (Boxplot/Barras)"), value=TRUE),
        selectInput("custom_theme", "Tema:", choices=c("Clássico","Minimalista","Preto e Branco","Claro","Vazio"), selected="Clássico"),
        selectInput("custom_palette", "Paleta de Cores:", choices=c("FIP606 (Padrão)","Viridis","Set1","Set2","Dark2","Pastel1","Oceano","Tropical", "Tons de Cinza", "Preto e Branco"), selected="FIP606 (Padrão)"),
        sliderInput("custom_font_size", "Tamanho da Fonte:", min=10, max=22, value=14, step=1),
        sliderInput("custom_pt_size", "Tamanho dos Pontos:", min=1, max=5, value=2.5, step=0.5),
        radioButtons("custom_x_angle", "Ângulo do Eixo X:", choices=c("0°"="0", "45°"="45", "90°"="90"), inline=TRUE),
        selectInput("custom_legend_pos", "Posição da Legenda:", choices=c("top","bottom","left","right","none"), selected="top"),
        sliderInput("custom_alpha", "Transparência (alpha):", min=0.1, max=1.0, value=0.65, step=0.05),
        numericInput("custom_y_min", "Limite Mínimo (Y):", value=NA),
        numericInput("custom_y_max", "Limite Máximo (Y):", value=NA)
      ),
      accordion_panel(title=tags$span(icon("magnifying-glass-plus"), icon("tag"), " Rótulos Customizados"), value="rotulos",
        textInput("custom_title", "Título do Gráfico:", placeholder="(usar padrão da aba)"),
        textInput("custom_xlab",  "Rótulo Eixo X:", placeholder="(usar padrão da aba)"),
        textInput("custom_ylab",  "Rótulo Eixo Y:", placeholder="(usar padrão da aba)"),
        textInput("custom_legend_title", "Título da Legenda (Fator Secundário):", placeholder="(usar padrão da aba)"),
        tags$hr(),
        tags$div(class="section-hint", "Para renomear grupos nos gráficos, digite os novos nomes separados por vírgula."),
        textInput("custom_levels_1", "Fator Principal (Eixo X / Trat 1):", placeholder="Ex: Controle, Dose 1, Dose 2"),
        textInput("custom_levels_2", "Fator Secundário (Cor / Trat 2):", placeholder="Ex: Fungicida A, Fungicida B")
      ),
      accordion_panel(title=tags$span(icon("magnifying-glass-plus"), icon("file-pdf"), " Exportação PDF"), value="pdf",
        numericInput("custom_pdf_w", "Largura (pol):", value=9, min=3, max=20, step=0.5),
        numericInput("custom_pdf_h", "Altura (pol):",  value=5.5, min=2, max=15, step=0.5)
      )
    )
  ),
  
  # ─── ABA INICIAL (BOAS VINDAS E UPLOAD) ─────────────────
  nav_panel(title=tags$span("Início"), value="aba_inicio",
    tags$div(class="top-banner", "Beta — nós estamos melhorando o FIP606 Studio. Dê o seu feedback ;)"),
    tags$div(class="rendr-container",
      tags$h1(class="rendr-title", "Solte um arquivo para começar a análise."),
      tags$p(class="rendr-subtitle", "Faça o upload da sua planilha (CSV ou Excel) e inicie suas análises e visualizações imediatamente."),
      
      tags$div(class="dashed-box",
        tags$div(class="dashed-box-title", "Solte um arquivo CSV ou Excel"),
        tags$div(class="dashed-box-sub", "ou clique para escolher um arquivo"),
        tags$div(class="file-badges",
          tags$span(class="file-badge", ".CSV"),
          tags$span(class="file-badge", ".XLSX")
        ),
        tags$div(class="file-limit", "Máx 20MB"),
        fileInput("upload_data", NULL, buttonLabel="", placeholder="", accept=c(".csv",".xlsx",".xls"))
      ),
      uiOutput("ui_sheet_selector"),
      uiOutput("ui_upload_summary"),
      
      tags$div(class="rendr-accordion",
        accordion(open = FALSE,
          accordion_panel(title=tags$span(icon("graduation-cap"), " Material das Aulas (FIP606)"), value="aulas",
            tags$p(style="margin-bottom: 15px;", "Baixe aqui os arquivos base (.qmd) utilizados nas aulas da disciplina:"),
            uiOutput("ui_aulas_download")
          ),
          accordion_panel(title="Dados de Exemplo (FIP606)",
            tags$div(class="row",
              tags$div(class="col-md-6",
                tags$div(class="example-mini-card d-flex align-items-center", tags$span(class="ex-icon", icon("map-location-dot")), tags$div(actionLink("btn_ex_mapa", tags$span(class="ex-title", "Mapas (Ceará)")))),
                tags$div(class="example-mini-card d-flex align-items-center", tags$span(class="ex-icon", icon("scale-balanced")), tags$div(actionLink("btn_ex_ttest", tags$span(class="ex-title", "Teste T (Produtividade)")))),
                tags$div(class="example-mini-card d-flex align-items-center", tags$span(class="ex-icon", icon("layer-group")), tags$div(actionLink("btn_ex_anova", tags$span(class="ex-title", "ANOVA (Fungicida Vaso)"))))
              ),
              tags$div(class="col-md-6",
                tags$div(class="example-mini-card d-flex align-items-center", tags$span(class="ex-icon", icon("bug")), tags$div(actionLink("btn_ex_glm", tags$span(class="ex-title", "GLM (Insetos)")))),
                tags$div(class="example-mini-card d-flex align-items-center", tags$span(class="ex-icon", icon("chart-line")), tags$div(actionLink("btn_ex_reg", tags$span(class="ex-title", "Regressão (Nitrogênio)")))),
                tags$div(class="example-mini-card d-flex align-items-center", tags$span(class="ex-icon", icon("virus")), tags$div(actionLink("btn_ex_audpc", tags$span(class="ex-title", "AUDPC (Curvas de Progresso)")))),
                tags$div(class="example-mini-card d-flex align-items-center", tags$span(class="ex-icon", icon("project-diagram")), tags$div(actionLink("btn_ex_cor", tags$span(class="ex-title", "Correlação (Produtividade e doença)"))))
              )
            )
          )
        )
      ),
      tags$hr(style="margin-top: 15px; border-color: #E0E0E0;"),
      tags$p(style="text-align: center; font-size: 0.8rem; color: #9AA0A6; margin-bottom: 5px;",
        icon("laptop-code"), " Desenvolvido na disciplina FIP 606 — Mestrado em Fitopatologia (UFV)"),
      tags$p(style="text-align: center; font-size: 0.78rem; color: #9AA0A6; margin-bottom: 0;",
        "Prof. Emerson Medeiros Del Ponte | Alunos: Milton E. C. M. Galvino & Laura G. Agudelo")
    )
  ),
  
  # ─── ABA PASSO A PASSO (DIDÁTICA) ──────────────────────────
  nav_panel(title=tags$span(icon("list-ol"), " Passo a Passo"), value="aba_passos",
    tags$div(class="container", style="max-width: 900px; margin-top: 30px; margin-bottom: 50px;",
      tags$h2(style="color: #1A73E8; font-weight: bold; margin-bottom: 20px;", icon("route"), " O Caminho da Análise Estatística"),
      tags$p(style="font-size: 1.1rem; color: #5F6368; margin-bottom: 30px;", 
             "Um guia definitivo baseado nas aulas da disciplina FIP 606 para você conduzir análises rigorosas e modernas do começo ao fim."),
      
      accordion(open = c("passo1", "passo2", "passo3", "passo4", "passo5", "passo6", "passo7"), multiple = TRUE,
        accordion_panel(title=tags$span(style="font-weight: 600; font-size: 1.1rem;", "1. Curadoria e Preparação dos Dados (Wrangling)"), value="passo1", icon=icon("broom"),
          tags$p("Antes de qualquer matemática, os dados precisam estar organizados e padronizados."),
          tags$ul(
            tags$li(HTML("<b>Importação:</b> Carregar os dados (via <code>read_csv</code> ou <code>read_excel</code>).")),
            tags$li(HTML("<b>Limpeza:</b> Usar funções do <code>dplyr</code> (como <code>rename</code>, <code>mutate</code>, <code>case_match</code>) para corrigir nomes e valores.")),
            tags$li("Garantir que as variáveis categóricas (como Blocos e Tratamentos) estejam interpretadas como Fatores no R.")
          ),
          tags$div(style="margin-top: 15px;", actionButton("btn_passo_dados", "1. Fazer Upload de Dados", class="btn-primary", icon=icon("upload")))
        ),
        
        accordion_panel(title=tags$span(style="font-weight: 600; font-size: 1.1rem;", "2. Análise Exploratória Visual (AED)"), value="passo2", icon=icon("magnifying-glass-chart"),
          tags$p("A regra de ouro: sempre 'olhe para os dados' antes de tentar modelá-los."),
          tags$ul(
            tags$li(HTML("Criar gráficos exploratórios usando <code>ggplot2</code>. A sobreposição de <code>geom_boxplot</code> com <code>geom_jitter</code> é fortemente recomendada para não esconder a distribuição real dos dados.")),
            tags$li(HTML("Em experimentos fatoriais, use facetamento (<code>facet_wrap</code>) ou o <code>interaction.plot()</code> para buscar indícios visuais de interação antes mesmo da ANOVA."))
          ),
          tags$div(style="margin-top: 15px;", actionButton("btn_passo_aed", "2. Ir para Análise Exploratória", class="btn-primary", icon=icon("magnifying-glass-chart")))
        ),
        
        accordion_panel(title=tags$span(style="font-weight: 600; font-size: 1.1rem;", "3. Escolha e Ajuste do Modelo Estatístico"), value="passo3", icon=icon("gears"),
          tags$p("Ajuste o modelo que espelha exatamente a forma como o experimento foi conduzido no campo ou na bancada:"),
          tags$ul(
            tags$li(HTML("<b>DIC ou 2 Grupos:</b> <code>lm(Y ~ Trat)</code> ou <code>t.test</code>.")),
            tags$li(HTML("<b>DBC (Blocos Casuais):</b> Isolando a heterogeneidade da área: <code>lm(Y ~ Trat + Bloco)</code>.")),
            tags$li(HTML("<b>Fatorial:</b> Avaliando efeitos principais e a interação simultaneamente: <code>lm(Y ~ FatorA * FatorB)</code>.")),
            tags$li(HTML("<b>Modelos Mistos:</b> Para restrições de aleatorização (parcelas subdivididas) use o pacote <code>lme4</code>: <code>lmer(Y ~ Trat + (1|Bloco))</code>.")),
            tags$li(HTML("<b>Associação (Correlação):</b> Avaliando a relação entre múltiplas variáveis contínuas numéricas."))
          ),
          tags$div(style="margin-top: 15px;", 
            actionButton("btn_passo_ttest", "Teste T", class="btn-outline-primary mb-1", icon=icon("scale-balanced")),
            actionButton("btn_passo_anova", "ANOVA / Mistos", class="btn-outline-primary mb-1", icon=icon("layer-group")),
            actionButton("btn_passo_reg", "Regressão", class="btn-outline-primary mb-1", icon=icon("chart-line")),
            actionButton("btn_passo_cor", "Correlação", class="btn-outline-primary mb-1", icon=icon("project-diagram")),
            actionButton("btn_passo_glm", "GLM (Contagens)", class="btn-outline-primary mb-1", icon=icon("bug"))
          )
        ),
        
        accordion_panel(title=tags$span(style="font-weight: 600; font-size: 1.1rem;", "4. Diagnóstico e Verificação de Premissas"), value="passo4", icon=icon("stethoscope"),
          tags$p("A inferência da ANOVA só é confiável se os resíduos do modelo passarem nos testes estatísticos."),
          tags$ul(
            tags$li(HTML("<b>A Abordagem Moderna:</b> Use o pacote <code>DHARMa</code> (via <code>simulateResiduals</code>) para um diagnóstico visual imbatível e fácil de interpretar.")),
            tags$li(HTML("<b>Testes Clássicos:</b> Shapiro-Wilk (para testar a Normalidade) e Bartlett/Levene (Homocedasticidade/Variâncias).")),
            tags$li(HTML("<b>E se falhar?</b> Tente aplicar transformações matemáticas (ex: <code>sqrt</code>) ou avance para os <b>Modelos Lineares Generalizados (GLM)</b> (via <code>glm()</code>) se for lidar com contagens (Poisson/Binomial Negativa)."))
          ),
          tags$p(style="margin-top: 15px; font-size: 0.9em; color: #666;", icon("info-circle"), " No FIP606 Studio, os resíduos e o DHARMa são calculados automaticamente e exibidos dentro da aba de cada Teste Estatístico (ANOVA, GLM, etc).")
        ),
        
        accordion_panel(title=tags$span(style="font-weight: 600; font-size: 1.1rem;", "5. Análise de Variância (Quadro da ANOVA)"), value="passo5", icon=icon("table"),
          tags$p("A hora da verdade estatística."),
          tags$ul(
            tags$li(HTML("Rode a função <code>anova(modelo)</code>.")),
            tags$li("Verifique os p-valores globais na tabela para descobrir se os seus tratamentos realmente surtiram efeito ou se a interação fatorial foi significativa.")
          ),
          tags$p(style="margin-top: 15px; font-size: 0.9em; color: #666;", icon("info-circle"), " No FIP606 Studio, as tabelas de ANOVA e Deviance são geradas instantaneamente nas abas de ANOVA e GLM.")
        ),
        
        accordion_panel(title=tags$span(style="font-weight: 600; font-size: 1.1rem;", "6. Comparações Múltiplas (Agrupamento)"), value="passo6", icon=icon("object-group"),
          tags$p("Se (e somente se) a ANOVA apontou um p < 0.05, descubra 'quem é diferente de quem'."),
          tags$ul(
            tags$li(HTML("Use o pacote <code>emmeans</code> para extrair as médias puras (ou médias ajustadas caso use Covariáveis/Modelos Mistos) diretamente do seu modelo.")),
            tags$li(HTML("Use o pacote <code>multcomp</code> (função <code>cld</code>) para rodar o teste de médias (como Tukey) e gerar as famosas letras de significância estatística (ex: 'a', 'ab', 'b')."))
          ),
          tags$p(style="margin-top: 15px; font-size: 0.9em; color: #666;", icon("info-circle"), " No FIP606 Studio, o teste de médias é feito automaticamente usando o emmeans nas abas de ANOVA e GLM.")
        ),
        
        accordion_panel(title=tags$span(style="font-weight: 600; font-size: 1.1rem;", "7. Comunicação e Visualização Científica"), value="passo7", icon=icon("chart-line"),
          tags$p("Hora de montar o gráfico de nível de publicação (paper) e escrever a interpretação dos resultados."),
          tags$ul(
            tags$li(HTML("Retorne ao <code>ggplot2</code> combinando tudo: dados brutos (<code>geom_jitter</code> opaco), barras de Margem de Erro Clássicas ou Intervalo de Confiança 95% (<code>geom_errorbar</code>), as estimativas do modelo (<code>geom_point</code>) e as letras do teste de médias flutuando no topo (<code>geom_text</code>).")),
            tags$li(HTML("Arremate o gráfico com temas elegantes voltados para publicação, como o <code>theme_classic()</code>."))
          ),
          tags$div(style="margin-top: 15px;", actionButton("btn_passo_export", "Ir para Exportação de Gráficos", class="btn-primary", icon=icon("camera")))
        )
      )
    )
  ),
  
  # ─── ABA 0: DADOS (OCULTA INICIALMENTE) ─────────────────
  nav_panel(title=tags$span(icon("table")," Dados"), value="aba_dados",
    layout_sidebar(sidebar=sidebar(width=340,
      tags$h5(icon("wrench"), " Curadoria (Wrangling)"),
      uiOutput("ui_sheet_selector_dados"),
      
      # ── Verbos dplyr: select ──────────────────────────────
      tags$h6(icon("columns"), " select() — Selecionar Colunas", class="mt-2 text-primary", style="font-weight: 600;"),
      uiOutput("ui_select_cols"),
      actionButton("btn_select_cols", "Aplicar select()", class="btn-sm btn-outline-primary w-100 mb-2"),
      
      hr(),
      
      # ── Verbos dplyr: filter ─────────────────────────────
      tags$h6(icon("filter"), " filter() — Filtrar Linhas", class="mt-2 text-primary", style="font-weight: 600;"),
      uiOutput("ui_filter_col"),
      uiOutput("ui_filter_vals"),
      actionButton("btn_filter_vals", "Aplicar filter()", class="btn-sm btn-outline-primary w-100 mb-2"),
      
      hr(),
      
      # ── Tratar NAs ──────────────────────────────
      tags$h6(icon("eraser"), " Tratar NAs", class="mt-2 text-danger", style="font-weight: 600;"),
      tags$div(class="section-hint", "Remove todas as linhas que possuam algum valor ausente (NA) na tabela inteira."),
      actionButton("btn_drop_na", "Remover Linhas com NA (drop_na)", class="btn-sm btn-outline-danger w-100 mb-2"),
      
      hr(),
      
      # ── Converter Tipos ──────────────────────────────
      tags$h6(icon("right-left"), " Converter Tipos (mutate)", class="mt-2 text-success", style="font-weight: 600;"),
      tags$div(class="section-hint", "Transforma colunas identificadas incorretamente como Numéricas para Categóricas (Fator)."),
      uiOutput("ui_convert_col"),
      actionButton("btn_convert_factor", "Forçar como Fator (as.factor)", class="btn-sm btn-outline-success w-100 mb-2"),
      
      hr(),
      
      # ── Corrigir escrita de valores ──────────────────────
      tags$h6(icon("spell-check"), " Corrigir Valores (recode)", class="mt-2 text-warning", style="font-weight: 600;"),
      tags$div(class="section-hint", "Renomeie valores dentro de uma coluna (ex: corrigir erros de digitação)."),
      uiOutput("ui_recode_col"),
      uiOutput("ui_recode_old"),
      textInput("recode_new", "Novo valor:", placeholder="Ex: Controle"),
      actionButton("btn_recode", " Corrigir Valor", icon = icon("pen"), class="btn-sm btn-outline-warning w-100 mb-2"),
      
      hr(),
      
      # ── Tipo de coluna ────────────────────────────────────
      tags$h6(icon("code"), " Tipo de Coluna", class="mt-2", style="font-weight: 600;"),
      uiOutput("ui_wrang_col"),
      actionButton("btn_to_factor", "Forçar Fator (Grupos)", class="btn-sm btn-outline-primary w-100 mb-2"),
      actionButton("btn_to_numeric", "Forçar Numérico", class="btn-sm btn-outline-success w-100 mb-3"),
      hr(),
      tags$h6(icon("arrows-left-right"), " Largo para Longo", class="mt-2 text-primary", style="font-weight: 600;"),
      uiOutput("ui_pivot_cols"),
      textInput("pivot_name", "Nova Coluna de Nomes:", value = "Variavel"),
      textInput("pivot_value", "Nova Coluna de Valores:", value = "Valor"),
      actionButton("btn_pivot", "Transformar Tabela", class="btn-sm btn-info w-100 mb-3"),
      hr(),
      tags$div(class="section-hint", "Selecione linhas na tabela à direita para exclui-las do banco ativo."),
      actionButton("btn_remove_row", "Remover Linhas Selecionadas", class="btn-sm btn-danger w-100 mb-2"),
      actionButton("btn_reset_data", "Restaurar Banco Original", class="btn-sm btn-warning w-100")
    ),
    card(card_header(tags$span(icon("table")," Dados Ativos")), DTOutput("tabela_dados")),
    card(card_header(tags$span(icon("calculator")," Estatísticas Descritivas")), DTOutput("tabela_desc_stats"))
    )
  ),
  # ─── ABA 1: AED ──────────────────────────────────────────────────────────
  nav_panel(title=tags$span(icon("magnifying-glass-chart")," Análise Exploratória de Dados"), value="aba_aed",
    layout_sidebar(sidebar=sidebar(
      uiOutput("ui_aed_trat1"),
      uiOutput("ui_aed_trat2"),
      uiOutput("ui_aed_resp"),
      uiOutput("ui_aed_bloco"),
      tags$div(class="warn-dic", icon("circle-info"), HTML(' Se <b>"Nenhum"</b> for selecionado no Bloco, o modelo será ajustado como <b>DIC</b>. Se selecionado, será <b>DBC</b>.')),
      hr(),
      checkboxInput("aed_use_facet", tags$b("Facetar por Tratamento 2 (Fatorial)"), value=FALSE),
      checkboxInput("aed_show_patchwork", tags$b("Combinar com Resumo (Média ± SE)"), value=FALSE),
      selectInput("transf_aed", tags$span(icon("arrows-rotate"), " Transformação (Y):"),
                  choices=transf_choices, selected="Nenhuma"),
      uiOutput("transf_aed_hint")
    ),
    navset_card_tab(
      nav_panel(tags$span(icon("chart-area")," Gráfico Exploratório"),
        div(class="d-flex justify-content-end p-2 export-btn-group",
          downloadButton("dl_plot_aed", tags$span(icon("file-pdf")," PDF"), class="btn-sm btn-outline-secondary"),
          downloadButton("dl_plot_aed_png", tags$span(icon("file-image")," PNG"), class="btn-sm btn-outline-secondary"),
          downloadButton("dl_plot_aed_svg", tags$span(icon("file-code")," SVG"), class="btn-sm btn-outline-secondary")
        ),
        uiOutput("dyn_plot_aed")),
      
      nav_panel(tags$span(icon("chart-simple")," Uniformidade (DHARMa)"),
        uiOutput("ui_aed_uniformity_badge"),
        withSpinner(plotOutput("plot_aed_qq_dharma", height="400px"), type=6, color="#18BC9C")
      ),

      nav_panel(tags$span(icon("scale-balanced")," Dispersão (DHARMa)"),
        uiOutput("ui_aed_dispersion_badge"),
        withSpinner(plotOutput("plot_aed_resfit_dharma", height="400px"), type=6, color="#18BC9C")
      ),

      nav_panel(tags$span(icon("terminal")," Resultados Brutos"),
        verbatimTextOutput("res_aed_pressupostos")),
      nav_panel(tags$span(icon("circle-info")," Sobre"),
        tags$div(class="module-about",
          h5(icon("magnifying-glass-chart")," Análise Exploratória de Dados"),
          p("Selecione as variáveis para gerar boxplots, dispersões e correlações de Pearson automaticamente."),
          tags$span("Selecione uma variável Categórica no Eixo X e uma Numérica no Eixo Y para criar um Boxplot. Se escolher duas variáveis Numéricas, o sistema criará um gráfico de Dispersão e calculará automaticamente a correlação de Pearson!"),
          tags$hr(),
          tags$span(HTML("<b>Pressupostos:</b> Selecione Tratamento 1, Tratamento 2 (Fatorial), Resposta e Bloco para ajustar um modelo linear e testar <b>homogeneidade das variâncias</b> (Bartlett e Levene) e <b>normalidade dos resíduos</b> (Shapiro-Wilk).")),
          tags$hr(),
          tags$div(class="conceptual-explanation",
            h6(icon("book"), " Entendendo os Conceitos:"),
            tags$ul(
              tags$li(HTML("<b>Boxplot:</b> Excelente para visualizar a distribuição dos dados, mostrando a mediana, os quartis e potenciais <i>outliers</i> (valores atípicos).")),
              tags$li(HTML("<b>Gráfico de Dispersão:</b> Mostra a relação geral entre duas variáveis contínuas num plano bidimensional.")),
              tags$li(HTML("<b>Correlação de Pearson (r):</b> Mede a força e a direção da relação linear entre variáveis. Varia de -1 (inversa perfeita) a 1 (direta perfeita). O valor 0 indica ausência de relação linear.")),
              tags$li(HTML("<b>Pressupostos Paramétricos:</b> Para muitos testes avançados, a estatística exige que a variância dentro dos grupos seja semelhante (Homocedasticidade) e que os resíduos do modelo sigam a curva normal."))
            )
          )
        )
      )
    ))
  ),
  
  # ─── ABA 2: TESTE T ────────────────────────────────────
  nav_panel(title=tags$span(icon("scale-balanced")," Teste T"), value="aba_ttest",
    layout_sidebar(sidebar=sidebar(
      uiOutput("ui_ttest_group"), uiOutput("ui_ttest_resp"),
      hr(),
      # ── Escolha do teste ──────────────────────────────────
      radioButtons("ttest_method", tags$span(icon("flask"), " Teste a usar:"),
        choices = c(
          "Automático (Shapiro decide)" = "auto",
          "Teste T (paramétrico)" = "ttest",
          "Wilcoxon/Mann-Whitney (não-paramétrico)" = "wilcoxon"
        ),
        selected = "auto"
      ),
      hr(),
      # ── Critério de Normalidade ───────────────────────
      tags$h6(icon("chart-simple"), " Normalidade", style="font-weight:600; margin-bottom:4px;"),
      selectInput("ttest_norm_crit", NULL,
        choices = c(
          "Shapiro-Wilk (Rigoroso)" = "shapiro",
          "Teorema Central do Limite (N > 30)" = "tcl",
          "Uniformidade DHARMa (KS)" = "dharma"
        ),
        selected = "shapiro"
      ),
      uiOutput("ui_ttest_shapiro_badge"),
      hr(),
      selectInput("transf_ttest", tags$span(icon("arrows-rotate"), " Transformação (Y):"),
                  choices=transf_choices, selected="Nenhuma"),
      uiOutput("transf_ttest_hint")
    ),

    navset_card_tab(id = "navset_ttest",

      nav_panel(title=tags$span(icon("box")," Boxplot"), value="graph_ttest",
        div(class="d-flex justify-content-end p-2 export-btn-group",
          downloadButton("dl_plot_ttest", tags$span(icon("file-pdf")," PDF"), class="btn-sm btn-outline-secondary"),
          downloadButton("dl_plot_ttest_png", tags$span(icon("file-image")," PNG"), class="btn-sm btn-outline-secondary"),
          downloadButton("dl_plot_ttest_svg", tags$span(icon("file-code")," SVG"), class="btn-sm btn-outline-secondary")
        ),
        uiOutput("dyn_plot_ttest")),
      nav_panel(tags$span(icon("terminal")," Resultados Brutos"),
        div(class="d-flex justify-content-end p-2", 
            actionButton("btn_copy_ttest", tags$span(icon("clipboard")," Copiar Relatório Completo"), class="btn-sm btn-outline-primary me-2"),
            downloadButton("dl_res_ttest", tags$span(icon("file-lines")," TXT"), class="btn-sm btn-outline-secondary")
        ),
        verbatimTextOutput("res_ttest")),
      nav_panel(tags$span(icon("lightbulb")," Interpretação"), uiOutput("interp_ttest")),
      nav_panel(tags$span(icon("quote-left")," Texto Científico"), uiOutput("sci_ttest")),
      nav_panel(tags$span(icon("circle-info")," Sobre"),
        tags$div(class="module-about", h5(icon("scale-balanced")," Testes de Hipóteses (T / Wilcoxon)"),
                 p("Compare 2 grupos com escolha explícita do teste ou modo automático via Shapiro-Wilk."),
                 tags$span(HTML("A variável de Grupo deve possuir exatamente 2 níveis (ex: Presença/Ausência).<br><br>
                   <b>Automático:</b> O aplicativo testa a normalidade dos resíduos — se p&gt;0.05, usa Teste T; caso contrário, Wilcoxon.<br>
                   <b>Teste T:</b> Paramétrico — assume normalidade e variâncias iguais.<br>
                   <b>Wilcoxon (Mann-Whitney U):</b> Não-paramétrico — compara as distribuições/medianas sem assumir normalidade.")),
                 tags$hr(),
                 tags$div(class="conceptual-explanation",
                   h6(icon("book"), " Entendendo os Conceitos:"),
                   tags$ul(
                     tags$li(HTML("<b>Teste T de Student:</b> É usado para descobrir se há uma diferença real entre as médias de dois grupos. Ele calcula se a diferença observada é grande o suficiente para não ser mero acaso, levando em conta a dispersão (variância) dos dados.")),
                     tags$li(HTML("<b>Teste de Wilcoxon (Mann-Whitney):</b> Quando os dados são muito assimétricos ou não normais (e não temos uma amostra grande), a média engana. Este teste ranqueia todos os dados (do menor pro maior) e compara se um grupo tende a ter ranks maiores que o outro.")),
                     tags$li(HTML("<b>Teorema Central do Limite (TCL):</b> Princípio que afirma que, em amostras grandes (geralmente N > 30), a média se comporta de forma normal, permitindo usar o Teste T com segurança, mesmo se os dados originais não forem estritamente normais."))
                   )
                 )
        )
      ),
      nav_panel(tags$span(icon("database")," Carregar Exemplo"), tags$div(class="d-flex flex-column justify-content-center align-items-center text-center", style="min-height: 400px;", tags$h4("Precisa de dados para testar?"), tags$p("Clique no botão abaixo para carregar um conjunto de dados didático e preencher a análise automaticamente."), actionButton("btn_ex_ttest_tab", "Carregar Exemplo: Teste T", icon=icon("download"), class="btn-primary btn-lg mt-3")))
    ))
  ),
  
  # ─── ABA 3: ANOVA ──────────────────────────────────────
  nav_panel(title=tags$span(icon("layer-group")," ANOVA"), value="aba_anova",
    layout_sidebar(sidebar=sidebar(
      uiOutput("ui_anova_trat1"), uiOutput("ui_anova_trat2"),
      uiOutput("ui_anova_resp"), uiOutput("ui_anova_bloco"),
      uiOutput("ui_anova_bloco_rand"),
      tags$div(class="warn-dic", icon("circle-info"), HTML(' Se <b>"Nenhum"</b> for selecionado no Bloco, o modelo será ajustado como <b>DIC</b>. Se selecionado, será <b>DBC</b>.')),
      hr(),
      radioButtons("anova_method", "Método:", choices=c("Paramétrico (ANOVA)", "Não-Paramétrico (Kruskal/Friedman)")),
      radioButtons("anova_norm_crit", "Critério de Validação:", choices=c("Clássico (Shapiro/Levene)"="classico", "Moderno (DHARMa)"="dharma")),
      uiOutput("ui_anova_posthoc"),
      # ── Badge de Premissas (automático) ──
      uiOutput("ui_anova_premissas_badge"),
      hr(),
      selectInput("transf_anova", tags$span(icon("arrows-rotate"), " Transformação (Y):"),
                  choices=transf_choices, selected="Nenhuma"),
      uiOutput("transf_anova_hint")
    ),

    navset_card_tab(id = "navset_anova",

      nav_panel(title=tags$span(icon("chart-column")," Médias (Tukey)"), value="graph_anova",
        div(class="d-flex justify-content-end p-2 export-btn-group",
          downloadButton("dl_plot_anova", tags$span(icon("file-pdf")," PDF"), class="btn-sm btn-outline-secondary"),
          downloadButton("dl_plot_anova_png", tags$span(icon("file-image")," PNG"), class="btn-sm btn-outline-secondary"),
          downloadButton("dl_plot_anova_svg", tags$span(icon("file-code")," SVG"), class="btn-sm btn-outline-secondary")
        ),
        uiOutput("dyn_plot_anova_tukey")),
      nav_panel(tags$span(icon("terminal")," Resultados Brutos"),
        div(class="d-flex justify-content-end p-2", 
            actionButton("btn_copy_anova", tags$span(icon("clipboard")," Copiar Relatório Completo"), class="btn-sm btn-outline-primary me-2"),
            downloadButton("dl_res_anova", tags$span(icon("file-lines")," TXT"), class="btn-sm btn-outline-secondary")
        ),
        verbatimTextOutput("res_anova")
      ),
      nav_panel(tags$span(icon("table-cells")," Tabela Fatorial"), uiOutput("ui_tbl_fatorial_anova")),
      nav_panel(tags$span(icon("stethoscope")," Diagnóstico DHARMa"), withSpinner(plotOutput("plot_anova_dharma", height="480px"), type=6, color="#18BC9C")),
      nav_panel(tags$span(icon("lightbulb")," Interpretação"), uiOutput("interp_anova")),
      nav_panel(tags$span(icon("quote-left")," Texto Científico"), uiOutput("sci_anova")),
      nav_panel(tags$span(icon("circle-info")," Sobre"),
        tags$div(class="module-about", h5(icon("layer-group")," ANOVA — Fatorial e Blocos"),
                 p("Defina os fatores, a resposta e o bloco. O app gera ANOVA, médias Tukey e DHARMa."),
                 tags$span(HTML("<b>Tratamento 1:</b> Fator principal. <b>Tratamento 2:</b> Deixe em 'Nenhum' para ANOVA simples ou adicione para Fatorial Duplo. <b>Bloco:</b> Adiciona o fator bloco ao modelo (DBC). O diagnóstico DHARMa ajuda a validar a adequação dos resíduos.")),
                 tags$hr(),
                 tags$div(class="conceptual-explanation",
                   h6(icon("book"), " Entendendo os Conceitos:"),
                   tags$ul(
                     tags$li(HTML("<b>ANOVA (Análise de Variância):</b> Extensão do Teste T para 3 ou mais grupos. Particiona a variação total em duas partes: a variação <i>entre</i> os tratamentos e a variação <i>dentro</i> dos grupos (erro aleatório). Se a variação <i>entre</i> grupos for substancialmente maior, o tratamento teve efeito significativo.")),
                     tags$li(HTML("<b>Delineamento em Blocos Casualizados (DBC):</b> Usado quando há heterogeneidade na área experimental (ex: declividade, sombreamento). O fator 'bloco' isola essa variação indesejada, melhorando a precisão do teste dos tratamentos.")),
                     tags$li(HTML("<b>Arranjo Fatorial:</b> Avalia não só o efeito isolado de dois tratamentos (ex: Fungicida e Dose), mas também a sua <i>interação</i> (se o efeito de aumentar a dose é o mesmo dependendo do fungicida).")),
                     tags$li(HTML("<b>Teste de Tukey (Post-hoc):</b> A ANOVA apenas alerta se 'existe alguma diferença geral'. O Tukey compara exaustivamente as médias duas a duas para mostrar exatamente <i>quem</i> difere de <i>quem</i>, controlando a taxa de erro inflacionada pelas múltiplas comparações."))
                   )
                 )
        )
      ),
      nav_panel(tags$span(icon("database")," Carregar Exemplo"), tags$div(class="d-flex flex-column justify-content-center align-items-center text-center", style="min-height: 400px;", tags$h4("Precisa de dados para testar?"), tags$p("Clique no botão abaixo para carregar um conjunto de dados didático e preencher a análise automaticamente."), actionButton("btn_ex_anova_tab", "Carregar Exemplo: ANOVA", icon=icon("download"), class="btn-primary btn-lg mt-3")))
    ))
  ),
  
  # ─── ABA 4: GLM ────────────────────────────────────────
  nav_panel(title=tags$span(icon("bug")," GLM"), value="aba_glm",
    layout_sidebar(sidebar=sidebar(
      uiOutput("ui_glm_trat"), uiOutput("ui_glm_trat2"), uiOutput("ui_glm_resp"),
      hr(),
      selectInput("glm_family", tags$span(icon("chart-pie"), " Família (Distribuição):"),
                  choices=c("Poisson", "Quasipoisson", "Binomial Negativa"), selected="Poisson"),
      uiOutput("ui_glm_offset"),
      # ── Badge de Premissas GLM (DHARMa) ──
      uiOutput("ui_glm_premissas_badge"),
      hr(),
      tags$div(class="warn-dic", icon("circle-info"),
        HTML(' <b>Poisson:</b> Contagens sem sobredispersão. <b>Quasipoisson:</b> Corrige sobredispersão. <b>Binomial Negativa:</b> Alternativa formal para sobredispersão.'))
    ),

    navset_card_tab(id = "navset_glm",

      nav_panel(title=tags$span(icon("chart-column")," Taxas Estimadas"), value="graph_glm",
        div(class="d-flex justify-content-end p-2 export-btn-group",
          downloadButton("dl_plot_glm", tags$span(icon("file-pdf")," PDF"), class="btn-sm btn-outline-secondary"),
          downloadButton("dl_plot_glm_png", tags$span(icon("file-image")," PNG"), class="btn-sm btn-outline-secondary"),
          downloadButton("dl_plot_glm_svg", tags$span(icon("file-code")," SVG"), class="btn-sm btn-outline-secondary")
        ),
        uiOutput("dyn_plot_glm_medias")),
      nav_panel(tags$span(icon("terminal")," Resultados Brutos"),
        div(class="d-flex justify-content-end p-2", 
            actionButton("btn_copy_glm", tags$span(icon("clipboard")," Copiar Relatório Completo"), class="btn-sm btn-outline-primary me-2"),
            downloadButton("dl_res_glm", tags$span(icon("file-lines")," TXT"), class="btn-sm btn-outline-secondary")
        ),
        verbatimTextOutput("res_glm")),
      nav_panel(tags$span(icon("stethoscope")," Diagnóstico DHARMa"), withSpinner(plotOutput("plot_glm_dharma", height="480px"), type=6, color="#18BC9C")),
      nav_panel(tags$span(icon("lightbulb")," Interpretação"), uiOutput("interp_glm")),
      nav_panel(tags$span(icon("quote-left")," Texto Científico"), uiOutput("sci_glm")),
      nav_panel(tags$span(icon("circle-info")," Sobre"),
        tags$div(class="module-about", h5(icon("bug")," Modelos Lineares Generalizados (GLM)"),
                 p("Para dados discretos (contagens). Suporta Poisson, Quasipoisson e Binomial Negativa."),
                 tags$span(HTML("<b>Poisson:</b> Modelo padrão para contagens. <b>Quasipoisson:</b> Ajusta erros-padrão quando há sobredispersão. <b>Binomial Negativa:</b> Alternativa formal com parâmetro extra de dispersão via MASS::glm.nb(). Use <b>Offset</b> para padronizar contagens por unidade (ex: insetos/planta). O <b>Tratamento 2</b> permite análise fatorial.")),
                 tags$hr(),
                 tags$div(class="conceptual-explanation",
                   h6(icon("book"), " Entendendo os Conceitos:"),
                   tags$ul(
                     tags$li(HTML("<b>GLM:</b> Quando os dados não são contínuos (ex: contagem de insetos: 0, 1, 2... ou doentes vs sadios), modelos baseados na curva normal falham estruturalmente. O GLM usa funções de ligação (link functions) para modelar esses dados em suas distribuições naturais.")),
                     tags$li(HTML("<b>Distribuição Poisson:</b> Distribuição clássica para contagens. Seu principal pressuposto (e limitação) é exigir que a média seja perfeitamente igual à variância.")),
                     tags$li(HTML("<b>Sobredispersão:</b> Na biologia, é quase uma regra que a variância seja bem maior que a média (ocorrência de muitos zeros ou muitos insetos agrupados). Se ignorado, o teste gera 'falsos positivos'.")),
                     tags$li(HTML("<b>Modelos Alternativos:</b> O <b>Quasipoisson</b> corrige a sobredispersão matematicamente multiplicando os erros-padrão. Já a <b>Binomial Negativa</b> acomoda a variação excessiva de forma estritamente estatística através de um parâmetro adicional."))
                   )
                 )
        )
      ),
      nav_panel(tags$span(icon("database")," Carregar Exemplo"), tags$div(class="d-flex flex-column justify-content-center align-items-center text-center", style="min-height: 400px;", tags$h4("Precisa de dados para testar?"), tags$p("Clique no botão abaixo para carregar um conjunto de dados didático e preencher a análise automaticamente."), actionButton("btn_ex_glm_tab", "Carregar Exemplo: GLM", icon=icon("download"), class="btn-primary btn-lg mt-3")))
    ))
  ),
  # ─── ABA CORRELAÇÃO ────────────────────────────────────
  nav_panel(title=tags$span(icon("project-diagram")," Correlação"), value="aba_cor",
    layout_sidebar(sidebar=sidebar(
      tags$h5(icon("sliders"), " Configurações"),
      uiOutput("ui_cor_cols"),
      selectInput("cor_method", tags$span(icon("calculator"), " Método:"), 
                  choices=c("Pearson (Linear)"="pearson", "Spearman (Postos)"="spearman")),
      tags$div(class="warn-dic", icon("info-circle"), " Pearson avalia relações lineares. Spearman avalia relações monotônicas e é robusto a outliers.")
    ),
    navset_card_underline(id = "navset_cor",

      nav_panel(title=tags$span(icon("chart-pie")," Resultados"), value="graph_cor",
        tags$div(class="res-card",
          tags$h5(icon("chart-pie"), " Matriz de Correlação (Correlograma)"),
          plotOutput("plot_cor", height="550px") |> withSpinner(color="#1A73E8")
        ),
        tags$div(class="res-card mt-3",
          tags$h5(icon("table"), " Coeficientes e p-valores"),
          DTOutput("tbl_cor") |> withSpinner(color="#1A73E8")
        )
      ),
      nav_panel(tags$span(icon("circle-info")," Sobre a Análise"),
        tags$div(class="module-about",
          h5(icon("project-diagram")," Correlação (Pearson vs Spearman)"),
          p("A correlação mede a força e a direção da associação entre duas variáveis numéricas."),
          tags$ul(
            tags$li(HTML("<b>Pearson:</b> Mede a associação <i>linear</i>. Pressupõe que os dados tenham distribuição normal e é sensível a valores extremos (outliers).")),
            tags$li(HTML("<b>Spearman:</b> Mede a associação <i>monotônica</i> (baseada em postos). É uma alternativa não paramétrica, mais robusta a outliers e distribuições assimétricas."))
          ),
          tags$hr(),
          h5("Interpretação da Magnitude (Guia Geral)"),
          tags$table(class="table table-bordered table-sm mt-2", style="max-width: 500px;",
            tags$thead(tags$tr(tags$th("Valor Absoluto (|r|)"), tags$th("Magnitude da Correlação"))),
            tags$tbody(
              tags$tr(tags$td("0.00 a 0.19"), tags$td("Muito Fraca")),
              tags$tr(tags$td("0.20 a 0.39"), tags$td("Fraca")),
              tags$tr(tags$td("0.40 a 0.69"), tags$td("Moderada")),
              tags$tr(tags$td("0.70 a 0.89"), tags$td("Forte")),
              tags$tr(tags$td("0.90 a 1.00"), tags$td("Muito Forte / Perfeita"))
            )
          ),
          p(tags$small(class="text-muted", "Nota: O coeficiente de correlação não implica causalidade. O p-valor (< 0.05) indica apenas se a correlação existente é significativamente diferente de zero."))
        )
      ),
      nav_panel(tags$span(icon("database")," Carregar Exemplo"), tags$div(class="d-flex flex-column justify-content-center align-items-center text-center", style="min-height: 400px;", tags$h4("Precisa de dados para testar?"), tags$p("Clique no botão abaixo para carregar um conjunto de dados didático e preencher a análise automaticamente."), actionButton("btn_ex_cor_tab", "Carregar Exemplo: Correlação", icon=icon("download"), class="btn-primary btn-lg mt-3")))
    ))
  ),
  
  # ─── ABA 5: REGRESSAO ──────────────────────────────────
  nav_panel(title=tags$span(icon("chart-line")," Regressão"), value="aba_reg",
    layout_sidebar(sidebar=sidebar(
      uiOutput("ui_reg_x"), uiOutput("ui_reg_y"),
      radioButtons("reg_type", "Modelo:", choices=c("Linear","Quadrática (Polinomial grau 2)")),
      conditionalPanel(
        condition = "input.reg_type == 'Quadrática (Polinomial grau 2)'",
        checkboxInput("reg_show_opt", tags$b("Mostrar caixa do ponto ótimo"), value = TRUE)
      ),
      checkboxInput("reg_show_eq", tags$b("Mostrar equação no gráfico"), value = FALSE),
      hr(),
      selectInput("transf_reg", tags$span(icon("arrows-rotate"), " Transformação (Y):"),
                  choices=transf_choices, selected="Nenhuma"),
      uiOutput("transf_reg_hint")
    ),

    navset_card_tab(id = "navset_reg",

      nav_panel(title=tags$span(icon("bezier-curve")," Curva Ajustada"), value="graph_reg",
        div(class="d-flex justify-content-end p-2 export-btn-group",
          downloadButton("dl_plot_reg", tags$span(icon("file-pdf")," PDF"), class="btn-sm btn-outline-secondary"),
          downloadButton("dl_plot_reg_png", tags$span(icon("file-image")," PNG"), class="btn-sm btn-outline-secondary"),
          downloadButton("dl_plot_reg_svg", tags$span(icon("file-code")," SVG"), class="btn-sm btn-outline-secondary")
        ),
        uiOutput("dyn_plot_reg")),
      nav_panel(tags$span(icon("stethoscope")," Diagnóstico (Resíduos)"),
        withSpinner(plotOutput("plot_reg_diag", height="520px"), type=6, color="#18BC9C")
      ),
      nav_panel(tags$span(icon("terminal")," Resultados Brutos"),
        div(class="d-flex justify-content-end p-2", 
            actionButton("btn_copy_reg", tags$span(icon("clipboard")," Copiar Relatório Completo"), class="btn-sm btn-outline-primary me-2"),
            downloadButton("dl_res_reg", tags$span(icon("file-lines")," TXT"), class="btn-sm btn-outline-secondary")
        ),
        verbatimTextOutput("res_reg")),
      nav_panel(tags$span(icon("lightbulb")," Interpretação"), uiOutput("interp_reg")),
      nav_panel(tags$span(icon("quote-left")," Texto Científico"), uiOutput("sci_reg")),
      nav_panel(tags$span(icon("circle-info")," Sobre"),
        tags$div(class="module-about", h5(icon("chart-line")," Regressão Linear e Polinomial"),
                 p("Ajuste modelos aos seus dados contínuos. Na quadrática, o ponto ótimo é calculado."),
                 tags$span("As variáveis X e Y precisam ser contínuas numéricas. Na regressão quadrática, o sistema calcula e interpreta automaticamente o ponto de máximo (ou mínimo) da curva parabólica ajustada."),
                 tags$hr(),
                 tags$div(class="conceptual-explanation",
                   h6(icon("book"), " Entendendo os Conceitos:"),
                   tags$ul(
                     tags$li(HTML("<b>Regressão Linear:</b> Avalia quantitativamente como uma variável (Y) muda em função de outra (X). Ao invés de comparar médias nominais, ela estima uma equação (Y = a + bX) indicando a taxa de crescimento/decréscimo.")),
                     tags$li(HTML("<b>R² (Coeficiente de Determinação):</b> Revela a % da variabilidade em Y que é matematicamente explicada pelo X. Um R² de 0.85 indica que 85% do fenômeno pode ser justificado pelo modelo linear estabelecido.")),
                     tags$li(HTML("<b>Regressão Quadrática:</b> Permite o ajuste de curvas (parábolas) quando a relação não é constante. É a ferramenta padrão na agronomia para descobrir a <b>Dose Ótima Técnica</b> (ponto de máximo, onde o rendimento para de subir e pode declinar por toxidez ou competição)."))
                   )
                 )
        )
      ),
      nav_panel(tags$span(icon("database")," Carregar Exemplo"), tags$div(class="d-flex flex-column justify-content-center align-items-center text-center", style="min-height: 400px;", tags$h4("Precisa de dados para testar?"), tags$p("Clique no botão abaixo para carregar um conjunto de dados didático e preencher a análise automaticamente."), actionButton("btn_ex_reg_tab", "Carregar Exemplo: Regressão", icon=icon("download"), class="btn-primary btn-lg mt-3")))
    ))
  ),
  
  # ─── ABA DE MAPAS ────────────────────────────────────────
  nav_panel(title=tags$span(icon("map-location-dot")," Mapas"), value="aba_mapa",
    layout_sidebar(sidebar=sidebar(
      uiOutput("ui_mapa_lon"),
      uiOutput("ui_mapa_lat"),
      uiOutput("ui_mapa_var"),
      uiOutput("ui_mapa_modo"),
      hr(),
      uiOutput("ui_mapa_regiao"),
      hr(),
      tags$div(class="warn-dic", icon("circle-info"), HTML(" O <b>Mapa</b> usa as colunas de coordenadas para plotar os dados na região selecionada. 'Automático' centralizará o foco direto nos pontos."))
    ),
    navset_card_tab(id = "navset_mapa",

      nav_panel(title=tags$span(icon("map")," Mapa Geográfico"), value="graph_mapa",
        uiOutput("mapa_info_paises"),
        div(class="d-flex justify-content-end p-2 export-btn-group",
          downloadButton("dl_plot_mapa", tags$span(icon("file-pdf")," PDF"), class="btn-sm btn-outline-secondary"),
          downloadButton("dl_plot_mapa_png", tags$span(icon("file-image")," PNG"), class="btn-sm btn-outline-secondary"),
          downloadButton("dl_plot_mapa_svg", tags$span(icon("file-code")," SVG"), class="btn-sm btn-outline-secondary")
        ),
        uiOutput("dyn_plot_mapa")),
      nav_panel(tags$span(icon("circle-info")," Sobre"),
        tags$div(class="module-about", h5(icon("map-location-dot")," Mapas e Georreferenciamento"),
                 p("Visualize a distribuição espacial de dados numéricos pelo mapa."),
                 tags$span(HTML("Esta aba utiliza os pacotes <b>sf</b> e <b>rnaturalearth</b> ensinados na aula para desenhar as fronteiras base e sobrepor as coordenadas coletadas em seu experimento.")),
                 tags$hr(),
                 tags$div(class="conceptual-explanation",
                   h6(icon("book"), " Entendendo os Conceitos:"),
                   tags$ul(
                     tags$li(HTML("<b>Georreferenciamento:</b> O ato de plotar variáveis quantitativas com base em suas coordenadas geográficas (Latitude e Longitude). É a base da Epidemiologia Espacial para detecção de reboleiras e focos.")),
                     tags$li(HTML("<b>Eixo X (Longitude):</b> As linhas meridionais (Leste/Oeste). É mapeado diretamente no eixo horizontal.")),
                     tags$li(HTML("<b>Eixo Y (Latitude):</b> As linhas paralelas (Norte/Sul). É mapeado no eixo vertical do globo.")),
                     tags$li(HTML("<b>Mapas de Bolhas (Bubble Maps):</b> Método em que se varia a cor e o raio do ponto na coordenada plotada de acordo com o nível da doença (Z). Bolhas maiores e mais quentes indicam o epicentro de uma infecção na lavoura."))
                   )
                 )
        )
      ),
      nav_panel(tags$span(icon("database")," Carregar Exemplo"), tags$div(class="d-flex flex-column justify-content-center align-items-center text-center", style="min-height: 400px;", tags$h4("Precisa de dados para testar?"), tags$p("Clique no botão abaixo para carregar um conjunto de dados didático e preencher a análise automaticamente."), actionButton("btn_ex_mapa_tab", "Carregar Exemplo: Mapas", icon=icon("download"), class="btn-primary btn-lg mt-3")))
    ))
  ),
  
  # ─── ABA 6: AUDPC ──────────────────────────────────────
  nav_panel(title=tags$span(icon("virus")," AUDPC"), value="aba_audpc",
    layout_sidebar(sidebar=sidebar(
      uiOutput("ui_audpc_time"), uiOutput("ui_audpc_sev"),
      uiOutput("ui_audpc_trat"), uiOutput("ui_audpc_rep"),
      hr(),
      checkboxInput("use_raudpc", tags$b("Usar AUDPC Relativa (rAUDPC)"), value=FALSE),
      selectInput("transf_audpc", tags$span(icon("arrows-rotate"), " Transf. AUDPC (barras):"),
                  choices=transf_choices, selected="Nenhuma"),
      uiOutput("transf_audpc_hint")
    ),

    navset_card_tab(id = "navset_audpc",

      nav_panel(title=tags$span(icon("chart-area")," Curva de Progresso (Área)"), value="graph_audpc",
        div(class="d-flex justify-content-end p-2 export-btn-group",
          downloadButton("dl_plot_audpc_curve", tags$span(icon("file-pdf")," PDF"), class="btn-sm btn-outline-secondary"),
          downloadButton("dl_plot_audpc_curve_png", tags$span(icon("file-image")," PNG"), class="btn-sm btn-outline-secondary"),
          downloadButton("dl_plot_audpc_curve_svg", tags$span(icon("file-code")," SVG"), class="btn-sm btn-outline-secondary")
        ),
        uiOutput("dyn_plot_audpc_curve")),
      nav_panel(tags$span(icon("chart-line")," Modelos Epidemiológicos"),
        tags$div(class="p-2 mb-2 bg-light border rounded", radioButtons("epi_model", "Modelo:", choices=c("Monomolecular", "Logístico", "Gompertz"), inline=TRUE)),
        div(class="d-flex justify-content-end p-2 export-btn-group",
          downloadButton("dl_plot_epi", tags$span(icon("file-pdf")," PDF"), class="btn-sm btn-outline-secondary"),
          downloadButton("dl_plot_epi_png", tags$span(icon("file-image")," PNG"), class="btn-sm btn-outline-secondary"),
          downloadButton("dl_plot_epi_svg", tags$span(icon("file-code")," SVG"), class="btn-sm btn-outline-secondary")
        ),
        uiOutput("dyn_plot_epi"),
        verbatimTextOutput("res_epi")),
      nav_panel(tags$span(icon("chart-column")," AUDPC — Médias (Tukey)"),
        div(class="d-flex justify-content-end p-2 export-btn-group",
          downloadButton("dl_plot_audpc_bar", tags$span(icon("file-pdf")," PDF"), class="btn-sm btn-outline-secondary"),
          downloadButton("dl_plot_audpc_bar_png", tags$span(icon("file-image")," PNG"), class="btn-sm btn-outline-secondary"),
          downloadButton("dl_plot_audpc_bar_svg", tags$span(icon("file-code")," SVG"), class="btn-sm btn-outline-secondary")
        ),
        uiOutput("dyn_plot_audpc_bar")),
      nav_panel(tags$span(icon("terminal")," Resultados Brutos"),
        div(class="d-flex justify-content-end p-2", 
            actionButton("btn_copy_audpc", tags$span(icon("clipboard")," Copiar Relatório Completo"), class="btn-sm btn-outline-primary me-2"),
            downloadButton("dl_res_audpc", tags$span(icon("file-lines")," TXT"), class="btn-sm btn-outline-secondary")
        ),
        verbatimTextOutput("res_audpc")),
      nav_panel(tags$span(icon("lightbulb")," Interpretação"), uiOutput("interp_audpc")),
      nav_panel(tags$span(icon("quote-left")," Texto Científico"), uiOutput("sci_audpc")),
      nav_panel(tags$span(icon("circle-info")," Sobre"),
        tags$div(class="module-about", h5(icon("virus")," AUDPC — Área Abaixo da Curva de Progresso"),
                 p("Calcule e visualize a AUDPC. A área sombreada no gráfico representa a integral trapezoidal."),
                 tags$span(HTML("Tempo e Severidade devem ser colunas numéricas. Para o modelo Epidemiológico (Epifitter), o R ajusta linearizações da curva para estimar a Taxa de Progresso (<i>r</i>) e a Severidade Inicial (<i>y0</i>). Se 'rAUDPC' estiver ativado, a AUDPC calculada será relativa (dividida pela amplitude do tempo).")),
                 tags$hr(),
                 tags$div(class="conceptual-explanation",
                   h6(icon("book"), " Entendendo os Conceitos:"),
                   tags$ul(
                     tags$li(HTML("<b>AUDPC (Área Abaixo da Curva de Progresso da Doença):</b> Quando acompanhamos uma epidemia, medir a doença num único dia final é limitante. A AUDPC resolve isso unindo as medições ao longo do tempo como um gráfico de trapézios e calculando a área total abaixo da linha. Integra toda a 'história' de adoecimento da planta.")),
                     tags$li(HTML("<b>rAUDPC (Relativa):</b> É a área padronizada, dividida pelo total de dias de avaliação. Torna possível comparar um experimento avaliado por 60 dias diretamente com outro de 80 dias.")),
                     tags$li(HTML("<b>Taxa de Progresso (<i>r</i>):</b> Estima a rapidez diária da disseminação ou evolução da doença. Taxas altas indicam surtos explosivos.")),
                     tags$li(HTML("<b>Linearização:</b> As curvas epidemiológicas reais têm formato logístico ou monomolecular. A matemática do pacote Epifitter as transforma numa linha reta logarítmica para facilitar a extração do '<i>r</i>' através da inclinação da reta."))
                   )
                 )
        )
      ),
      nav_panel(tags$span(icon("database")," Carregar Exemplo"), tags$div(class="d-flex flex-column justify-content-center align-items-center text-center", style="min-height: 400px;", tags$h4("Precisa de dados para testar?"), tags$p("Clique no botão abaixo para carregar um conjunto de dados didático e preencher a análise automaticamente."), actionButton("btn_ex_audpc_tab", "Carregar Exemplo: AUDPC", icon=icon("download"), class="btn-primary btn-lg mt-3")))
    )
  ),
  
  nav_spacer(),
  
  tags$script(HTML("
    $(document).on('shiny:connected', function() {
      var interval = setInterval(function() {
        var btn = document.querySelector('.bslib-sidebar-toggle');
        if(btn) {
          btn.innerHTML = '<i class=\"fa-solid fa-bars\" style=\"font-size:36px; color:#000; margin-left:10px;\"></i>';
          clearInterval(interval);
        }
      }, 100);
    });
  "))
)
)

# ═══════════════════════════════════════════════════════════
#  SERVER
# ═══════════════════════════════════════════════════════════
server <- function(input, output, session) {
  
  get_qmd_title <- function(html_path) {
    qmd_path <- sub("\\.html$", ".qmd", html_path)
    if (file.exists(qmd_path)) {
      linhas <- readLines(qmd_path, n = 20, warn = FALSE)
      tit_linha <- grep("^title:", linhas, value = TRUE)
      if (length(tit_linha) > 0) {
        titulo <- sub("^title:\\s*[\"']?(.*?)[\"']?\\s*$", "\\1", tit_linha[1])
        titulo <- sub("^Aula\\s*(\\d+)", "Aula \\1", titulo, ignore.case = TRUE)
        return(titulo)
      }
    }
    return(basename(html_path))
  }
  
  # ─── MÓDULO: VISUALIZAÇÃO DE AULAS (.html) ───────────────────
  output$ui_aulas_download <- renderUI({
    # Procura arquivos .html na pasta 'aulas'
    arquivos <- list.files("aulas", pattern = "\\.html$", full.names = TRUE)
    
    if (length(arquivos) == 0) {
      return(tags$p(style="color: #9AA0A6; font-style: italic; font-size: 0.9rem;", 
                    icon("info-circle"), " Nenhum arquivo .html encontrado na pasta 'aulas'. (Renderize seus .qmd para .html e coloque-os nesta pasta)"))
    }
    
    # Cria os botões de visualização dinamicamente
    botoes <- lapply(seq_along(arquivos), function(i) {
      arq_path <- arquivos[i]
      btn_label <- get_qmd_title(arq_path)
      btn_id <- paste0("btn_aula_", i)
      
      tags$div(style="margin-bottom: 8px;",
        actionButton(btn_id, label = btn_label, icon = icon("book-open"),
                       class = "btn-secondary btn-sm", style="width: 100%; text-align: left; font-weight: 500;")
      )
    })
    
    do.call(tagList, botoes)
  })
  
  # Cria os observers de clique dinamicamente
  observe({
    arquivos <- list.files("aulas", pattern = "\\.html$", full.names = TRUE)
    lapply(seq_along(arquivos), function(i) {
      arq_path <- arquivos[i]
      arq_nome <- basename(arq_path)
      modal_title <- get_qmd_title(arq_path)
      btn_id <- paste0("btn_aula_", i)
      
      observeEvent(input[[btn_id]], {
        showModal(modalDialog(
          title = tags$strong(icon("book-open"), " ", modal_title),
          size = "xl",
          easyClose = TRUE,
          fade = TRUE,
          tags$iframe(src = paste0("aulas_html/", arq_nome), 
                      style = "width: 100%; height: 75vh; border: none; border-radius: 8px;")
        ))
      }, ignoreInit = TRUE)
    })
  })

  # Hide analysis tabs on startup
  nav_hide("main_nav", "aba_dados")
  nav_hide("main_nav", "aba_aed")
  nav_hide("main_nav", "aba_ttest")
  nav_hide("main_nav", "aba_anova")
  nav_hide("main_nav", "aba_glm")
  nav_hide("main_nav", "aba_cor")
  nav_hide("main_nav", "aba_reg")
  nav_hide("main_nav", "aba_mapa")
  nav_hide("main_nav", "aba_audpc")
  
  rv <- reactiveValues(
    raw_data=NULL, data=NULL, uploaded_file_path=NULL, uploaded_file_ext=NULL,
    aed_trat1="Nenhum", aed_trat2="Nenhum", aed_resp=NULL, aed_bloco="Nenhum",
    ttest_group=NULL, ttest_resp=NULL,
    anova_trat1=NULL, anova_trat2="Nenhum", anova_resp=NULL, anova_bloco="Nenhum",
    glm_trat=NULL, glm_trat2="Nenhum", glm_resp=NULL, glm_offset="Nenhum", reg_x=NULL, reg_y=NULL,
    audpc_time=NULL, audpc_sev=NULL, audpc_trat=NULL, audpc_rep=NULL,
    mapa_lon=NULL, mapa_lat=NULL, mapa_var="Nenhum", mapa_modo="Ambos (Cor e Tamanho)"
  )
  export <- reactiveValues(plot_aed=NULL,plot_gauss=NULL,plot_ttest=NULL,plot_anova=NULL,plot_glm=NULL,plot_reg=NULL,
                           plot_audpc_curve=NULL,plot_audpc_bar=NULL,plot_epi=NULL,plot_mapa=NULL,
                           res_ttest=NULL,res_anova=NULL,res_glm=NULL,res_reg=NULL,res_audpc=NULL,
                           txt_ttest=NULL,txt_anova=NULL,txt_glm=NULL,txt_reg=NULL,txt_audpc=NULL)
  

  
  # Logic to show/hide analysis tabs
  observeEvent(rv$data, {
    if (!is.null(rv$data)) {
      nav_show("main_nav", "aba_dados")
      nav_show("main_nav", "aba_aed")
      nav_show("main_nav", "aba_ttest")
      nav_show("main_nav", "aba_anova")
      nav_show("main_nav", "aba_glm")
      nav_show("main_nav", "aba_cor")
      nav_show("main_nav", "aba_reg")
      nav_show("main_nav", "aba_mapa")
      nav_show("main_nav", "aba_audpc")
    } else {
      nav_hide("main_nav", "aba_dados")
      nav_hide("main_nav", "aba_aed")
      nav_hide("main_nav", "aba_ttest")
      nav_hide("main_nav", "aba_anova")
      nav_hide("main_nav", "aba_glm")
      nav_hide("main_nav", "aba_cor")
      nav_hide("main_nav", "aba_reg")
      nav_hide("main_nav", "aba_mapa")
      nav_hide("main_nav", "aba_audpc")
    }
  })
  
  # Toggle da barra lateral baseado na aba ativa
  observeEvent(input$main_nav, {
    if (input$main_nav %in% c("aba_inicio", "aba_dados", "aba_passos")) {
      sidebar_toggle("global_sidebar", open = FALSE)
    } else {
      sidebar_toggle("global_sidebar", open = TRUE)
    }
    
    # Atualizar seções do relatório para a aba ativa
    tab_to_section <- c(
      "aba_aed" = "aed", "aba_ttest" = "ttest", "aba_anova" = "anova",
      "aba_glm" = "glm", "aba_cor" = "cor", "aba_reg" = "reg",
      "aba_audpc" = "audpc", "aba_mapa" = "mapa"
    )
    if (input$main_nav %in% names(tab_to_section)) {
      updateCheckboxGroupInput(session, "report_sections", selected = tab_to_section[[input$main_nav]])
    } else {
      updateCheckboxGroupInput(session, "report_sections", selected = character(0))
    }
  })
  
  # ─── Dynamic Plotly Renderers ─────────────────────────────
  render_dyn_plot <- function(id, h) {
    if(input$use_plotly) plotlyOutput(paste0("plotly_", id), height=h) else withSpinner(plotOutput(id, height=h), type=6, color="#18BC9C")
  }
  
  # ─── Gerador de Relatório RMarkdown ─────────────────────
  output$dl_report <- downloadHandler(
    filename = function() { ext <- if(input$report_format == "html") ".html" else ".docx"; paste0("Relatorio_FIP606_", format(Sys.time(), "%Y%m%d_%H%M"), ext) },
    content = function(file) {
      # Caminho robusto para report.Rmd (relativo ao diretorio do app)
      app_dir <- getwd()
      rmd_source <- file.path(app_dir, "report.Rmd")
      if (!file.exists(rmd_source)) {
        # Fallback: tenta diretorio do script
        rmd_source <- system.file("report.Rmd", package = .packageName, mustWork = FALSE)
        if (!file.exists(rmd_source)) rmd_source <- "report.Rmd"
      }
      
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy(rmd_source, tempReport, overwrite = TRUE)
      
      icon_source <- file.path(app_dir, "www", "app_icon.png")
      if (file.exists(icon_source)) {
        file.copy(icon_source, file.path(tempdir(), "app_icon.png"), overwrite = TRUE)
      }
      
      params <- list(
        title = input$report_title %||% "Relatório Estatístico e Epidemiológico",
        author = input$report_author %||% "Não Informado",
        models = list(
          ttest = tryCatch(export$model_ttest, error=function(e) NULL),
          anova = tryCatch(export$model_anova, error=function(e) NULL),
          glm   = tryCatch(export$model_glm, error=function(e) NULL),
          reg   = tryCatch(export$model_reg, error=function(e) NULL)
        ),
        plots = list(
          ttest = tryCatch(export$plot_ttest, error=function(e) NULL),
          anova = tryCatch(export$plot_anova, error=function(e) NULL),
          glm   = tryCatch(export$plot_glm, error=function(e) NULL),
          reg   = tryCatch(export$plot_reg, error=function(e) NULL),
          audpc_curve = tryCatch(export$plot_audpc_curve, error=function(e) NULL),
          audpc_bar   = tryCatch(export$plot_audpc_bar, error=function(e) NULL),
          epi   = tryCatch(export$plot_epi, error=function(e) NULL)
        ),
        texts = list(
          ttest = export$txt_ttest %||% NULL,
          anova = export$txt_anova %||% NULL,
          glm   = export$txt_glm %||% NULL,
          reg   = export$txt_reg %||% NULL,
          audpc = export$txt_audpc %||% NULL
        ),
        interp = list(
          ttest = tryCatch(export$interp_ttest_html, error=function(e) NULL),
          anova = tryCatch(export$interp_anova_html, error=function(e) NULL),
          glm   = tryCatch(export$interp_glm_html, error=function(e) NULL),
          reg   = tryCatch(export$interp_reg_html, error=function(e) NULL),
          audpc = tryCatch(export$interp_audpc_html, error=function(e) NULL)
        ),
        raw_results = list(
          ttest = export$res_ttest %||% NULL,
          anova = export$res_anova %||% NULL,
          glm   = export$res_glm %||% NULL,
          reg   = export$res_reg %||% NULL,
          audpc = export$res_audpc %||% NULL
        ),
        tables = list(
          anova = tryCatch(export$tbl_anova_fat, error=function(e) NULL),
          glm   = tryCatch(export$tbl_glm_fat, error=function(e) NULL)
        ),
        dharma = list(
          ttest = tryCatch(export$dharma_ttest, error=function(e) NULL),
          anova = tryCatch(export$dharma_anova, error=function(e) NULL),
          glm   = tryCatch(export$dharma_glm, error=function(e) NULL)
        ),
        cor_data = tryCatch(export$cor_data, error=function(e) NULL),
        cor_method = tryCatch(export$cor_method, error=function(e) NULL),
        dataset = rv$data,
        file_info = if(is.null(rv$uploaded_file_path)) "Conjunto de Dados de Exemplo Embutido" else "Arquivo de Usuário (Personalizado)",
        plot_aed = tryCatch(export$plot_aed, error=function(e) NULL),
        plot_mapa = tryCatch(export$plot_mapa, error=function(e) NULL),
        sections = input$report_sections,
        desc_stats = tryCatch(desc_stats(), error=function(e) NULL)
      )
      
      tryCatch({
        out_format <- if(input$report_format == "html") "html_document" else "word_document"
        withProgress(message = "Gerando relatório...", value = 0.3, {
          incProgress(0.3, detail = "Renderizando gráficos e tabelas...")
          rmarkdown::render(tempReport, output_format = out_format, output_file = file, params = params, envir = new.env(parent = globalenv()))
          incProgress(0.4, detail = "Finalizado!")
        })
      }, error = function(e) { print(e$message);
        showNotification(
          paste("Erro ao gerar relatório:", e$message),
          type = "error", duration = 10
        )
        # Gera um arquivo de fallback com o erro
        writeLines(c(
          "=== ERRO NA GERACAO DO RELATORIO ===",
          "",
          paste("Mensagem:", e$message),
          "",
          "Possiveis causas:",
          "  1. O pacote 'rmarkdown' nao esta instalado. Execute: install.packages('rmarkdown')",
          "  2. O Pandoc nao esta disponivel. Instale via: installr::install.pandoc()",
          "  3. O arquivo report.Rmd nao foi encontrado no diretorio do app.",
          "",
          paste("Diretorio do app:", app_dir),
          paste("report.Rmd existe?", file.exists(rmd_source))
        ), file)
      })
    }
  )

  output$dyn_plot_aed <- renderUI({ render_dyn_plot("plot_aed", "480px") })
  output$plotly_plot_aed <- renderPlotly({ req(export$plot_aed); ggplotly(export$plot_aed, tooltip="all") |> layout(hovermode="closest") })
  
  output$dyn_plot_gauss <- renderUI({ render_dyn_plot("plot_gauss", "480px") })
  output$plotly_plot_gauss <- renderPlotly({ req(export$plot_gauss); ggplotly(export$plot_gauss, tooltip="all") |> layout(hovermode="closest") })
  
  output$dyn_plot_ttest <- renderUI({ render_dyn_plot("plot_ttest", "400px") })
  output$plotly_plot_ttest <- renderPlotly({ req(export$plot_ttest); ggplotly(export$plot_ttest, tooltip="all") |> layout(hovermode="closest") })
  
  output$dyn_plot_anova_tukey <- renderUI({ render_dyn_plot("plot_anova_tukey", "480px") })
  output$plotly_plot_anova_tukey <- renderPlotly({ req(export$plot_anova); ggplotly(export$plot_anova, tooltip="all") |> layout(hovermode="closest") })
  
  output$dyn_plot_glm_medias <- renderUI({ render_dyn_plot("plot_glm_medias", "480px") })
  output$plotly_plot_glm_medias <- renderPlotly({ req(export$plot_glm); ggplotly(export$plot_glm, tooltip="all") |> layout(hovermode="closest") })
  
  output$dyn_plot_reg <- renderUI({ render_dyn_plot("plot_reg", "480px") })
  output$plotly_plot_reg <- renderPlotly({ req(export$plot_reg); ggplotly(export$plot_reg, tooltip="all") |> layout(hovermode="closest") })
  
  output$dyn_plot_audpc_curve <- renderUI({ render_dyn_plot("plot_audpc_curve", "500px") })
  output$plotly_plot_audpc_curve <- renderPlotly({ req(export$plot_audpc_curve); ggplotly(export$plot_audpc_curve, tooltip="all") |> layout(hovermode="closest") })
  
  output$dyn_plot_epi <- renderUI({ render_dyn_plot("plot_epi", "500px") })
  output$plotly_plot_epi <- renderPlotly({ req(export$plot_epi); ggplotly(export$plot_epi, tooltip="all") |> layout(hovermode="closest") })
  
  output$dyn_plot_audpc_bar <- renderUI({ render_dyn_plot("plot_audpc_bar", "480px") })
  output$plotly_plot_audpc_bar <- renderPlotly({ req(export$plot_audpc_bar); ggplotly(export$plot_audpc_bar, tooltip="all") |> layout(hovermode="closest") })

  output$dyn_plot_mapa <- renderUI({ render_dyn_plot("plot_mapa", "550px") })
  output$plotly_plot_mapa <- renderPlotly({ req(export$plot_mapa); ggplotly(export$plot_mapa, tooltip="all") |> layout(hovermode="closest") })

  # ─── Helpers reativos para personalizacao ────────────────
  cur_theme   <- reactive({ get_theme_func(input$custom_theme %||% "Clássico") })
  cur_palette <- reactive({ rep(get_palette_colors(input$custom_palette %||% "FIP606 (Padrão)"), length.out = 100) })
  cur_fsize   <- reactive({ input$custom_font_size %||% 14 })
  cur_alpha   <- reactive({ input$custom_alpha %||% 0.65 })
  cur_plot_type <- reactive({ input$custom_plot_type %||% "Boxplot" })
  cur_legend  <- reactive({ input$custom_legend_pos %||% "top" })
  cur_pdf_w   <- reactive({ input$custom_pdf_w %||% 9 })
  cur_pdf_h   <- reactive({ input$custom_pdf_h %||% 5.5 })
  cur_jitter  <- reactive({ input$custom_jitter %||% TRUE })
  cur_pt_size <- reactive({ input$custom_pt_size %||% 2.5 })
  
  # ─── Upload e Selecao de Abas Excel ──────────────────────
  observeEvent(input$upload_data, {
    rv$uploaded_file_path <- input$upload_data$datapath
    rv$uploaded_file_ext <- tools::file_ext(input$upload_data$name)
    if (rv$uploaded_file_ext == "csv") {
      tryCatch({ 
        d <- read_csv(rv$uploaded_file_path, show_col_types=FALSE)
        rv$raw_data <- d; rv$data <- d
        nav_select("main_nav", "aba_dados")
        showNotification("Dados carregados com sucesso!", type="message", duration=4)
      }, error=function(e) { rv$raw_data<-NULL; rv$data<-NULL })
    }
  })
  
  output$ui_sheet_selector <- renderUI({
    req(rv$uploaded_file_path, rv$uploaded_file_ext)
    if (rv$uploaded_file_ext %in% c("xls", "xlsx")) {
      sheets <- readxl::excel_sheets(rv$uploaded_file_path)
      selectInput("selected_sheet", "Selecione a Aba (Planilha):", choices = sheets)
    } else { NULL }
  })
  
  observeEvent(input$selected_sheet, {
    req(rv$uploaded_file_path, rv$uploaded_file_ext)
    if (rv$uploaded_file_ext %in% c("xls", "xlsx")) {
      tryCatch({ 
        d <- read_excel(rv$uploaded_file_path, sheet = input$selected_sheet)
        rv$raw_data <- d; rv$data <- d
        nav_select("main_nav", "aba_dados")
        showNotification("Dados carregados com sucesso!", type="message", duration=4)
      }, error=function(e) { rv$raw_data<-NULL; rv$data<-NULL })
    }
  })
  
  output$ui_sheet_selector_dados <- renderUI({
    req(rv$uploaded_file_path, rv$uploaded_file_ext)
    if (rv$uploaded_file_ext %in% c("xls", "xlsx")) {
      sheets <- readxl::excel_sheets(rv$uploaded_file_path)
      selectInput("selected_sheet_dados", "Selecione a Aba (Planilha):", choices = sheets, selected = input$selected_sheet)
    } else { NULL }
  })
  
  observeEvent(input$selected_sheet_dados, {
    req(rv$uploaded_file_path, rv$uploaded_file_ext)
    if (rv$uploaded_file_ext %in% c("xls", "xlsx")) {
      tryCatch({ 
        d <- read_excel(rv$uploaded_file_path, sheet = input$selected_sheet_dados)
        rv$raw_data <- d; rv$data <- d
      }, error=function(e) { rv$raw_data<-NULL; rv$data<-NULL })
    }
  })
  
  # ─── Cards de resumo pós-upload ─────────────────────────────
  output$ui_upload_summary <- renderUI({
    req(rv$data)
    df <- rv$data
    n_rows   <- nrow(df)
    n_cols   <- ncol(df)
    n_num    <- sum(sapply(df, is.numeric))
    n_fac    <- n_cols - n_num
    n_nas    <- sum(is.na(df))
    has_nas  <- n_nas > 0
    
    chip <- function(icon_name, label, extra_class = "") {
      tags$div(
        class = paste("summary-chip", extra_class),
        tags$span(class = "chip-icon", icon(icon_name)),
        label
      )
    }
    
    tags$div(
      class = "upload-summary",
      chip("table-cells",     paste0(n_rows, " linhas"),           "chip-ok"),
      chip("columns",         paste0(n_cols, " colunas"),          "chip-ok"),
      chip("hashtag",         paste0(n_num,  " numéricas"),        ""),
      chip("tag",             paste0(n_fac,  " fatores/texto"),    ""),
      chip(
        if (has_nas) "triangle-exclamation" else "circle-check",
        paste0(n_nas, " NAs no total"),
        if (has_nas) "chip-warn" else "chip-ok"
      )
    )
  })
  
  # ─── Exemplos FIP606 ───────────────────────────────────

  observeEvent(input$btn_ex_mapa,{ rv$uploaded_file_path<-NULL; d<-data.frame(Local=c('Fortaleza','Sobral','Juazeiro do Norte','Quixada','Iguatu','Crateus','Tiangua','Baturite','Crato','Itapipoca'), lat=c(-3.71,-3.68,-7.2,-4.97,-6.35,-5.17,-3.73,-4.32,-7.23,-3.49), lon=c(-38.52,-40.35,-39.31,-39.01,-39.29,-40.67,-40.99,-38.88,-39.41,-39.58), Incidencia=c(12,45,33,25,50,42,18,15,30,22)); rv$raw_data<-d; rv$data<-d; rv$mapa_lon<-"lon"; rv$mapa_lat<-"lat"; rv$mapa_var<-"Incidencia"; rv$mapa_regiao_selected<-"Ceará (Estado)"; nav_select("main_nav", "aba_mapa"); updateSelectInput(session, "mapa_regiao", selected="Ceará (Estado)"); showNotification("Dados de exemplo do Ceará carregados!", type="message", duration=4) })
  observeEvent(input$btn_ex_ttest,{ rv$uploaded_file_path<-NULL; d<-read_csv("data/experimento.csv"); rv$raw_data<-d; rv$data<-d; rv$ttest_group<-"treatment";rv$ttest_resp<-"yield_kg_ha"; nav_select("main_nav", "aba_ttest"); showNotification("Dados de exemplo carregados!", type="message", duration=4) })
  observeEvent(input$btn_ex_anova,{ rv$uploaded_file_path<-NULL; d<-read_excel("data/dados_diversos.xlsx","fungicida_vaso")|>mutate(incidencia=inf_seeds/n_seeds,dose=factor(dose)); rv$raw_data<-d; rv$data<-d; rv$anova_trat1<-"treat";rv$anova_trat2<-"dose";rv$anova_resp<-"severity";rv$anova_bloco<-"Nenhum"; nav_select("main_nav", "aba_anova"); showNotification("Dados de exemplo carregados!", type="message", duration=4) })
  observeEvent(input$btn_ex_glm,{ rv$uploaded_file_path<-NULL; d<-InsectSprays; rv$raw_data<-d; rv$data<-d; rv$glm_trat<-"spray";rv$glm_resp<-"count"; nav_select("main_nav", "aba_glm"); showNotification("Dados de exemplo carregados!", type="message", duration=4) })
  observeEvent(input$btn_ex_reg,{ rv$uploaded_file_path<-NULL; d<-data.frame(DOSEN=c(0,50,100,150,200,250),RG=c(7.1,7.3,7.66,7.71,7.62,7.6)); rv$raw_data<-d; rv$data<-d; rv$reg_x<-"DOSEN";rv$reg_y<-"RG"; nav_select("main_nav", "aba_reg"); showNotification("Dados de exemplo carregados!", type="message", duration=4) })
  observeEvent(input$btn_ex_audpc,{ rv$uploaded_file_path<-NULL; d<-read_excel("data/dados_diversos.xlsx","curve"); rv$raw_data<-d; rv$data<-d; rv$audpc_time<-"day";rv$audpc_sev<-"severity";rv$audpc_trat<-"Irrigation";rv$audpc_rep<-"rep"; nav_select("main_nav", "aba_audpc"); showNotification("Dados de exemplo carregados!", type="message", duration=4) })
  observeEvent(input$btn_ex_cor,{ rv$uploaded_file_path<-NULL; d<-data.frame(Produtividade=c(5554,4594,5797,5785,5194,5814,5929,5914,6620,4933,5704,6302,4772,4365,5731), Precipitacao=c(161,103,131,139,132,117,165,117,181,118,159,189,78,112,116), Temperatura=c(26.9,24.1,17,17.7,29,24.1,19.7,24.5,28.6,30.7,23.7,24.2,19.7,26.4,23.1), Severidade=c(93.2,74.1,75.2,62.2,88.1,54.1,71.2,63.3,73.1,81.8,85.3,89.4,60.5,65.9,55.8)); rv$raw_data<-d; rv$data<-d; nav_select("main_nav", "aba_cor"); showNotification("Dados agronômicos de exemplo carregados!", type="message", duration=4) })

  # Observadores para os botoes de exemplo dentro de cada aba de análise
  observeEvent(input$btn_ex_mapa_tab,{ rv$uploaded_file_path<-NULL; d<-data.frame(Local=c('Fortaleza','Sobral','Juazeiro do Norte','Quixada','Iguatu','Crateus','Tiangua','Baturite','Crato','Itapipoca'), lat=c(-3.71,-3.68,-7.2,-4.97,-6.35,-5.17,-3.73,-4.32,-7.23,-3.49), lon=c(-38.52,-40.35,-39.31,-39.01,-39.29,-40.67,-40.99,-38.88,-39.41,-39.58), Incidencia=c(12,45,33,25,50,42,18,15,30,22)); rv$raw_data<-d; rv$data<-d; rv$mapa_lon<-"lon"; rv$mapa_lat<-"lat"; rv$mapa_var<-"Incidencia"; rv$mapa_regiao_selected<-"Ceará (Estado)"; updateSelectInput(session, "mapa_regiao", selected="Ceará (Estado)"); nav_select("navset_mapa", "graph_mapa"); showNotification("Dados de exemplo do Ceará carregados!", type="message", duration=4) })
  observeEvent(input$btn_ex_ttest_tab,{ rv$uploaded_file_path<-NULL; d<-read_csv("data/experimento.csv"); rv$raw_data<-d; rv$data<-d; rv$ttest_group<-"treatment";rv$ttest_resp<-"yield_kg_ha"; nav_select("navset_ttest", "graph_ttest"); showNotification("Dados de exemplo carregados!", type="message", duration=4) })
  observeEvent(input$btn_ex_anova_tab,{ rv$uploaded_file_path<-NULL; d<-read_excel("data/dados_diversos.xlsx","fungicida_vaso")|>mutate(incidencia=inf_seeds/n_seeds,dose=factor(dose)); rv$raw_data<-d; rv$data<-d; rv$anova_trat1<-"treat";rv$anova_trat2<-"dose";rv$anova_resp<-"severity";rv$anova_bloco<-"Nenhum"; nav_select("navset_anova", "graph_anova"); showNotification("Dados de exemplo carregados!", type="message", duration=4) })
  observeEvent(input$btn_ex_glm_tab,{ rv$uploaded_file_path<-NULL; d<-InsectSprays; rv$raw_data<-d; rv$data<-d; rv$glm_trat<-"spray";rv$glm_resp<-"count"; nav_select("navset_glm", "graph_glm"); showNotification("Dados de exemplo carregados!", type="message", duration=4) })
  observeEvent(input$btn_ex_reg_tab,{ rv$uploaded_file_path<-NULL; d<-data.frame(DOSEN=c(0,50,100,150,200,250),RG=c(7.1,7.3,7.66,7.71,7.62,7.6)); rv$raw_data<-d; rv$data<-d; rv$reg_x<-"DOSEN";rv$reg_y<-"RG"; nav_select("navset_reg", "graph_reg"); showNotification("Dados de exemplo carregados!", type="message", duration=4) })
  observeEvent(input$btn_ex_audpc_tab,{ rv$uploaded_file_path<-NULL; d<-read_excel("data/dados_diversos.xlsx","curve"); rv$raw_data<-d; rv$data<-d; rv$audpc_time<-"day";rv$audpc_sev<-"severity";rv$audpc_trat<-"Irrigation";rv$audpc_rep<-"rep"; nav_select("navset_audpc", "graph_audpc"); showNotification("Dados de exemplo carregados!", type="message", duration=4) })
  observeEvent(input$btn_ex_cor_tab,{ rv$uploaded_file_path<-NULL; d<-data.frame(Produtividade=c(5554,4594,5797,5785,5194,5814,5929,5914,6620,4933,5704,6302,4772,4365,5731), Precipitacao=c(161,103,131,139,132,117,165,117,181,118,159,189,78,112,116), Temperatura=c(26.9,24.1,17,17.7,29,24.1,19.7,24.5,28.6,30.7,23.7,24.2,19.7,26.4,23.1), Severidade=c(93.2,74.1,75.2,62.2,88.1,54.1,71.2,63.3,73.1,81.8,85.3,89.4,60.5,65.9,55.8)); rv$raw_data<-d; rv$data<-d; nav_select("navset_cor", "graph_cor"); showNotification("Dados agronômicos de exemplo carregados!", type="message", duration=4) })
  
  # Action Buttons from Passo a Passo
  observeEvent(input$btn_passo_dados, { nav_select("main_nav", "aba_inicio") })
  observeEvent(input$btn_passo_aed, { nav_select("main_nav", "aba_aed") })
  observeEvent(input$btn_passo_ttest, { nav_select("main_nav", "aba_ttest") })
  observeEvent(input$btn_passo_anova, { nav_select("main_nav", "aba_anova") })
  observeEvent(input$btn_passo_reg, { nav_select("main_nav", "aba_reg") })
  observeEvent(input$btn_passo_cor, { nav_select("main_nav", "aba_cor") })
  observeEvent(input$btn_passo_glm, { nav_select("main_nav", "aba_glm") })
  observeEvent(input$btn_passo_export, { nav_select("main_nav", "aba_aed") ; showNotification("Você pode exportar gráficos usando os controles flutuantes nas próprias abas!", type="message", duration=5) })

  observeEvent(input$aed_trat1,{rv$aed_trat1<-input$aed_trat1}); observeEvent(input$aed_trat2,{rv$aed_trat2<-input$aed_trat2})
  observeEvent(input$aed_resp,{rv$aed_resp<-input$aed_resp}); observeEvent(input$aed_bloco,{rv$aed_bloco<-input$aed_bloco})
  observeEvent(input$ttest_group,{rv$ttest_group<-input$ttest_group}); observeEvent(input$ttest_resp,{rv$ttest_resp<-input$ttest_resp})
  observeEvent(input$anova_trat1,{rv$anova_trat1<-input$anova_trat1}); observeEvent(input$anova_trat2,{rv$anova_trat2<-input$anova_trat2})
  observeEvent(input$anova_resp,{rv$anova_resp<-input$anova_resp}); observeEvent(input$anova_bloco,{rv$anova_bloco<-input$anova_bloco})
  observeEvent(input$glm_trat,{rv$glm_trat<-input$glm_trat}); observeEvent(input$glm_resp,{rv$glm_resp<-input$glm_resp})
  observeEvent(input$glm_trat2,{rv$glm_trat2<-input$glm_trat2}); observeEvent(input$glm_offset,{rv$glm_offset<-input$glm_offset})
  observeEvent(input$reg_x,{rv$reg_x<-input$reg_x}); observeEvent(input$reg_y,{rv$reg_y<-input$reg_y})
  observeEvent(input$audpc_time,{rv$audpc_time<-input$audpc_time}); observeEvent(input$audpc_sev,{rv$audpc_sev<-input$audpc_sev})
  observeEvent(input$audpc_trat,{rv$audpc_trat<-input$audpc_trat}); observeEvent(input$audpc_rep,{rv$audpc_rep<-input$audpc_rep})
  
  col_names <- reactive({ req(rv$data); names(rv$data) })
  num_cols  <- reactive({ req(rv$data); names(rv$data)[sapply(rv$data,is.numeric)] })
  
  output$ui_aed_trat1<- renderUI(selectInput("aed_trat1","Tratamento 1:",c("Nenhum",col_names()),selected=rv$aed_trat1))
  output$ui_aed_trat2<- renderUI(selectInput("aed_trat2","Tratamento 2 (Fatorial):",c("Nenhum",col_names()),selected=rv$aed_trat2))
  output$ui_aed_resp <- renderUI(selectInput("aed_resp", "Resposta:", num_cols(), selected=rv$aed_resp))
  output$ui_aed_bloco<- renderUI(selectInput("aed_bloco","Bloco:",c("Nenhum",col_names()),selected=rv$aed_bloco))
  output$ui_ttest_group<-renderUI(selectInput("ttest_group","Grupo (2 níveis):",col_names(),selected=rv$ttest_group))
  output$ui_ttest_resp<-renderUI(selectInput("ttest_resp","Resposta:",num_cols(),selected=rv$ttest_resp))
  output$ui_anova_trat1<-renderUI(selectInput("anova_trat1","Tratamento 1:",col_names(),selected=rv$anova_trat1))
  output$ui_anova_trat2<-renderUI(selectInput("anova_trat2","Tratamento 2 (Fatorial):",c("Nenhum",col_names()),selected=rv$anova_trat2))
  output$ui_anova_resp<-renderUI(selectInput("anova_resp","Resposta:",num_cols(),selected=rv$anova_resp))
  output$ui_anova_bloco<-renderUI(selectInput("anova_bloco","Bloco:",c("Nenhum",col_names()),selected=rv$anova_bloco))
  
  output$ui_anova_bloco_rand <- renderUI({
    req(rv$anova_bloco != "Nenhum")
    checkboxInput("anova_bloco_rand", tags$b("Considerar bloco como efeito aleatório (lmer)"), value=FALSE)
  })
  
  output$ui_anova_posthoc <- renderUI({
    if(input$anova_method != "Paramétrico (ANOVA)") return(NULL)
    selectInput("anova_posthoc", "Teste de Médias:", choices=c("Tukey", "Fisher (LSD)", "Bonferroni"), selected="Tukey")
  })
  
  output$ui_glm_trat<-renderUI(selectInput("glm_trat","Tratamento 1:",col_names(),selected=rv$glm_trat))
  output$ui_glm_trat2<-renderUI(selectInput("glm_trat2","Tratamento 2 (Fatorial):",c("Nenhum",col_names()),selected=rv$glm_trat2))
  output$ui_glm_resp<-renderUI(selectInput("glm_resp","Contagem (Y):",num_cols(),selected=rv$glm_resp))
  output$ui_glm_offset<-renderUI(selectInput("glm_offset","Offset (opcional):",c("Nenhum",num_cols()),selected=rv$glm_offset))
  output$ui_reg_x<-renderUI(selectInput("reg_x","Var X:",num_cols(),selected=rv$reg_x))
  output$ui_reg_y<-renderUI(selectInput("reg_y","Var Y:",num_cols(),selected=rv$reg_y))
  output$ui_audpc_time<-renderUI(selectInput("audpc_time","Tempo (dias):",num_cols(),selected=rv$audpc_time))
  output$ui_audpc_sev<-renderUI(selectInput("audpc_sev","Severidade:",num_cols(),selected=rv$audpc_sev))
  output$ui_audpc_trat<-renderUI(selectInput("audpc_trat","Tratamento:",col_names(),selected=rv$audpc_trat))
  output$ui_audpc_rep<-renderUI(selectInput("audpc_rep","Repetição:",col_names(),selected=rv$audpc_rep))
  
  # ─── Hints de transformacao ──────────────────────────────
  output$transf_aed_hint   <- renderUI({ tt<-input$transf_aed;   if(is.null(tt)||tt=="Nenhuma") return(NULL); tags$div(class="transf-badge",icon("info-circle"),HTML(paste0(" Ativa: <b>",tt,"</b>"))) })
  output$transf_ttest_hint <- renderUI({ tt<-input$transf_ttest; if(is.null(tt)||tt=="Nenhuma") return(NULL); tags$div(class="transf-badge",icon("info-circle"),HTML(paste0(" Ativa: <b>",tt,"</b>"))) })
  output$transf_anova_hint <- renderUI({ tt<-input$transf_anova; if(is.null(tt)||tt=="Nenhuma") return(NULL); tags$div(class="transf-badge",icon("info-circle"),HTML(paste0(" Ativa: <b>",tt,"</b>"))) })
  output$transf_reg_hint   <- renderUI({ tt<-input$transf_reg;   if(is.null(tt)||tt=="Nenhuma") return(NULL); tags$div(class="transf-badge",icon("info-circle"),HTML(paste0(" Ativa: <b>",tt,"</b>"))) })
  output$transf_audpc_hint <- renderUI({ tt<-input$transf_audpc; if(is.null(tt)||tt=="Nenhuma") return(NULL); tags$div(class="transf-badge",icon("info-circle"),HTML(paste0(" Ativa: <b>",tt,"</b>"))) })
  
  # ─── Badge de Premissas da ANOVA (Shapiro-Wilk + Bartlett) ──────────────────
  output$ui_anova_premissas_badge <- renderUI({
    req(rv$data, rv$anova_trat1, rv$anova_resp)
    if (input$anova_method != "Paramétrico (ANOVA)") return(NULL)
    
    tt <- input$transf_anova %||% "Nenhuma"
    df <- rv$data
    if (tt != "Nenhuma") {
      df[["resp_transf"]] <- apply_transform(df[[rv$anova_resp]], tt)
    } else {
      df[["resp_transf"]] <- df[[rv$anova_resp]]
    }
    df[[rv$anova_trat1]] <- as.factor(df[[rv$anova_trat1]])
    
    m_tmp <- tryCatch(
      lm(as.formula(paste("resp_transf ~", rv$anova_trat1)), data = df),
      error = function(e) NULL
    )
    if (is.null(m_tmp)) return(NULL)
    
    crit <- input$anova_norm_crit %||% "classico"
    
    if (crit == "classico") {
      # ── Shapiro-Wilk nos resíduos
      sw  <- tryCatch(shapiro.test(resid(m_tmp)), error = function(e) list(p.value = NA))
      sw_p <- sw$p.value
      sw_ok <- !is.na(sw_p) && sw_p > 0.05
      sw_cls <- if (isTRUE(sw_ok)) "alert-success" else "alert-danger"
      sw_ico <- if (isTRUE(sw_ok)) icon("circle-check") else icon("circle-xmark")
      sw_msg <- if (is.na(sw_p))
        "Shapiro-Wilk: não calculado."
      else
        sprintf("Shapiro-Wilk: W = %.4f, p = %.4f — Resíduos %s.",
                sw$statistic, sw_p, if (sw_ok) "normais ✓" else "NÃO normais ✗")
      
      # ── Levene (homocedasticidade)
      fml <- as.formula(paste("resp_transf ~", rv$anova_trat1))
      lev <- tryCatch(
        car::leveneTest(fml, data = df),
        error = function(e) list(`Pr(>F)` = c(NA_real_, NA_real_), `F value` = c(NA_real_, NA_real_))
      )
      lev_p <- lev$`Pr(>F)`[1]
      lev_stat <- lev$`F value`[1]
      lev_ok <- !is.na(lev_p) && lev_p > 0.05
      lev_cls <- if (isTRUE(lev_ok)) "alert-success" else "alert-warning"
      lev_ico <- if (isTRUE(lev_ok)) icon("circle-check") else icon("triangle-exclamation")
      lev_msg <- if (is.na(lev_p))
        "Levene: não calculado."
      else
        sprintf("Levene: F = %.4f, p = %.4f — Variâncias %s.",
                lev_stat, lev_p, if (lev_ok) "homogêneas ✓" else "heterogêneas ✗")
      
      return(tags$div(
        style = "margin-top: 6px;",
        tags$div(class = paste("alert p-2 mb-1", sw_cls),  style = "font-size:0.78rem;", sw_ico,  " ", sw_msg),
        tags$div(class = paste("alert p-2 mb-0", lev_cls), style = "font-size:0.78rem;", lev_ico, " ", lev_msg)
      ))
      
    } else {
      # ── DHARMa (Uniformidade e Dispersão)
      sim_res <- tryCatch(DHARMa::simulateResiduals(m_tmp, n=250), error=function(e) NULL)
      if (is.null(sim_res)) return(tags$div(class="alert alert-warning p-2", "DHARMa: Falha na simulação."))
      
      unif <- tryCatch(DHARMa::testUniformity(sim_res, plot=FALSE), error=function(e) list(p.value=NA))
      disp <- tryCatch(DHARMa::testDispersion(sim_res, plot=FALSE), error=function(e) list(p.value=NA))
      
      sw_p <- unif$p.value
      sw_ok <- !is.na(sw_p) && sw_p > 0.05
      sw_cls <- if (isTRUE(sw_ok)) "alert-success" else "alert-danger"
      sw_ico <- if (isTRUE(sw_ok)) icon("circle-check") else icon("circle-xmark")
      sw_msg <- if(is.na(sw_p)) "DHARMa Uniformidade: Falha." else sprintf("DHARMa Uniformidade: p = %.4f — Resíduos %s.", sw_p, if(sw_ok) "adequados ✓" else "desviantes ✗")
                        
      lev_p <- disp$p.value
      lev_ok <- !is.na(lev_p) && lev_p > 0.05
      lev_cls <- if (isTRUE(lev_ok)) "alert-success" else "alert-warning"
      lev_ico <- if (isTRUE(lev_ok)) icon("circle-check") else icon("triangle-exclamation")
      lev_msg <- if(is.na(lev_p)) "DHARMa Dispersão: Falha." else sprintf("DHARMa Dispersão: p = %.4f — Variância %s.", lev_p, if(lev_ok) "adequada ✓" else "heterogênea ✗")
      
      return(tags$div(
        style = "margin-top: 6px;",
        tags$div(class = paste("alert p-2 mb-1", sw_cls),  style = "font-size:0.78rem;", sw_ico,  " ", sw_msg),
        tags$div(class = paste("alert p-2 mb-0", lev_cls), style = "font-size:0.78rem;", lev_ico, " ", lev_msg)
      ))
    }
  })

  # ─── Badge de Premissas do GLM (DHARMa) ──────────────────
  output$ui_glm_premissas_badge <- renderUI({
    req(rv$data, rv$glm_trat, rv$glm_resp)
    
    df <- rv$data
    resp <- rv$glm_resp
    trat1 <- rv$glm_trat
    trat2 <- rv$glm_trat2
    fam <- input$glm_family %||% "Poisson"
    off_col <- rv$glm_offset
    
    if(!is.numeric(df[[resp]])) return(NULL)
    
    df[[trat1]] <- as.factor(df[[trat1]])
    if(trat2 != "Nenhum") df[[trat2]] <- as.factor(df[[trat2]])
    
    fml_str <- if(trat2 != "Nenhum") paste(resp, "~", trat1, "*", trat2) else paste(resp, "~", trat1)
    if (off_col != "Nenhum" && is.numeric(df[[off_col]])) {
      df$log_off <- log(df[[off_col]] + 0.0001)
      fml_str <- paste(fml_str, "+ offset(log_off)")
    }
    fml <- as.formula(fml_str)
    
    m_tmp <- tryCatch({
      if (fam == "Poisson") glm(fml, data=df, family=poisson)
      else if (fam == "Quasipoisson") glm(fml, data=df, family=quasipoisson)
      else MASS::glm.nb(fml, data=df)
    }, error=function(e) NULL)
    
    if (is.null(m_tmp)) return(NULL)
    
    sim_res <- tryCatch(DHARMa::simulateResiduals(m_tmp, n=250), error=function(e) NULL)
    if (is.null(sim_res)) return(tags$div(class="alert alert-warning p-2", "DHARMa: Falha na simulação."))
    
    unif <- tryCatch(DHARMa::testUniformity(sim_res, plot=FALSE), error=function(e) list(p.value=NA))
    disp <- tryCatch(DHARMa::testDispersion(sim_res, plot=FALSE), error=function(e) list(p.value=NA))
    zinf <- tryCatch(DHARMa::testZeroInflation(sim_res, plot=FALSE), error=function(e) list(p.value=NA))
    
    unif_p <- unif$p.value; unif_ok <- !is.na(unif_p) && unif_p > 0.05
    disp_p <- disp$p.value; disp_ok <- !is.na(disp_p) && disp_p > 0.05
    zinf_p <- zinf$p.value; zinf_ok <- !is.na(zinf_p) && zinf_p > 0.05
    
    make_badge <- function(name, pval, is_ok, msg_ok, msg_fail, extra_mb="mb-1") {
      cls <- if(isTRUE(is_ok)) "alert-success" else "alert-warning"
      ico <- if(isTRUE(is_ok)) icon("circle-check") else icon("triangle-exclamation")
      msg <- if(is.na(pval)) paste(name, "Falha.") else sprintf("%s p=%.4f — %s", name, pval, if(is_ok) msg_ok else msg_fail)
      tags$div(class = paste("alert p-2", extra_mb, cls), style = "font-size:0.75rem;", ico, " ", msg)
    }
    
    tags$div(
      style = "margin-top: 6px;",
      make_badge("Uniformid.:", unif_p, unif_ok, "OK ✓", "Desviante ✗"),
      make_badge("Dispersão:", disp_p, disp_ok, "OK ✓", "Sobredispersão ✗"),
      make_badge("Zero-infl.:", zinf_p, zinf_ok, "OK ✓", "Excesso zeros ✗", "mb-0")
    )
  })
  
  # ══════════════════════════════════════════════════════════
  #  TAB 0 - DADOS BRUTOS
  # ══════════════════════════════════════════════════════════
  
  # ── select() — Selecionar Colunas ───────────────────────
  output$ui_select_cols <- renderUI({
    req(rv$data)
    selectizeInput("select_cols", "Manter colunas:", choices=names(rv$data), selected=names(rv$data), multiple=TRUE)
  })
  
  observeEvent(input$btn_select_cols, {
    req(rv$data, input$select_cols)
    if (length(input$select_cols) == 0) {
      showNotification("Selecione ao menos uma coluna.", type="warning"); return()
    }
    tryCatch({
      rv$data <- rv$data |> select(all_of(input$select_cols))
      showNotification(paste0("select(): ", length(input$select_cols), " colunas mantidas."), type="message")
    }, error=function(e) showNotification(paste("Erro no select():", e$message), type="error"))
  })
  
  # ── filter() — Filtrar Linhas ────────────────────────────
  output$ui_filter_col <- renderUI({
    req(rv$data)
    selectInput("filter_col", "Coluna para filtrar:", choices=names(rv$data))
  })
  
  output$ui_filter_vals <- renderUI({
    req(rv$data, input$filter_col)
    col <- rv$data[[input$filter_col]]
    vals <- sort(unique(na.omit(as.character(col))))
    selectizeInput("filter_vals", "Manter valores:", choices=vals, selected=vals, multiple=TRUE)
  })
  
  observeEvent(input$btn_filter_vals, {
    req(rv$data, input$filter_col, input$filter_vals)
    tryCatch({
      col_name <- input$filter_col
      keep_vals <- input$filter_vals
      n_before <- nrow(rv$data)
      rv$data <- rv$data |> filter(as.character(.data[[col_name]]) %in% keep_vals)
      n_after <- nrow(rv$data)
      showNotification(paste0("filter(): ", n_before - n_after, " linhas removidas. Restam ", n_after, " linhas."), type="message")
    }, error=function(e) showNotification(paste("Erro no filter():", e$message), type="error"))
  })
  
  # ── drop_na() — Tratar NAs ────────────────────────────
  observeEvent(input$btn_drop_na, {
    req(rv$data)
    tryCatch({
      n_before <- nrow(rv$data)
      rv$data <- tidyr::drop_na(rv$data)
      n_after <- nrow(rv$data)
      if (n_before == n_after) {
        showNotification("Nenhum NA encontrado nos dados atuais.", type="warning")
      } else {
        showNotification(paste0("drop_na(): ", n_before - n_after, " linhas com NAs removidas. Restam ", n_after, " linhas."), type="message")
      }
    }, error=function(e) showNotification(paste("Erro no drop_na():", e$message), type="error"))
  })

  # ── as.factor() — Converter Tipos ────────────────────────────
  output$ui_convert_col <- renderUI({
    req(rv$data)
    selectInput("convert_col", "Coluna para converter:", choices=names(rv$data))
  })
  
  observeEvent(input$btn_convert_factor, {
    req(rv$data, input$convert_col)
    tryCatch({
      col_name <- input$convert_col
      if(is.factor(rv$data[[col_name]])) {
         showNotification(paste0("A coluna '", col_name, "' já é um Fator."), type="warning")
         return()
      }
      rv$data[[col_name]] <- as.factor(rv$data[[col_name]])
      showNotification(paste0("as.factor(): Coluna '", col_name, "' convertida para Fator com sucesso."), type="message")
    }, error=function(e) showNotification(paste("Erro ao converter:", e$message), type="error"))
  })
  
  # ── Corrigir valores (recode) ────────────────────────────
  output$ui_recode_col <- renderUI({
    req(rv$data)
    selectInput("recode_col", "Coluna:", choices=names(rv$data))
  })
  
  output$ui_recode_old <- renderUI({
    req(rv$data, input$recode_col)
    col <- rv$data[[input$recode_col]]
    vals <- sort(unique(na.omit(as.character(col))))
    selectInput("recode_old", "Valor atual (a corrigir):", choices=vals)
  })
  
  observeEvent(input$btn_recode, {
    req(rv$data, input$recode_col, input$recode_old, input$recode_new)
    if (!nzchar(trimws(input$recode_new))) {
      showNotification("Digite o novo valor antes de corrigir.", type="warning"); return()
    }
    tryCatch({
      col <- rv$data[[input$recode_col]]
      was_factor <- is.factor(col)
      col_char <- as.character(col)
      col_char[col_char == input$recode_old] <- input$recode_new
      rv$data[[input$recode_col]] <- if (was_factor) as.factor(col_char) else col_char
      showNotification(paste0('recode(): "', input$recode_old, '" → "', input$recode_new, '" em "', input$recode_col, '".'), type="message")
    }, error=function(e) showNotification(paste("Erro no recode():", e$message), type="error"))
  })
  
  output$ui_wrang_col <- renderUI({
    req(rv$data)
    selectInput("wrang_col", "Selecione a Coluna:", choices=names(rv$data))
  })
  
  observeEvent(input$btn_to_factor, {
    req(rv$data, input$wrang_col)
    tryCatch({ rv$data[[input$wrang_col]] <- as.factor(rv$data[[input$wrang_col]]) }, error=function(e){})
  })
  
  observeEvent(input$btn_to_numeric, {
    req(rv$data, input$wrang_col)
    tryCatch({ rv$data[[input$wrang_col]] <- as.numeric(as.character(rv$data[[input$wrang_col]])) }, error=function(e){})
  })
  
  # --- Pivot Longer ---
  output$ui_pivot_cols <- renderUI({
    req(rv$data)
    selectizeInput("pivot_cols", "Selecione as Colunas para agrupar:", choices=names(rv$data), multiple=TRUE)
  })
  
  observeEvent(input$btn_pivot, {
    req(rv$data, input$pivot_cols, input$pivot_name, input$pivot_value)
    tryCatch({
      rv$data <- tidyr::pivot_longer(
        rv$data, 
        cols = all_of(input$pivot_cols), 
        names_to = input$pivot_name, 
        values_to = input$pivot_value
      )
      showNotification("Tabela transformada com sucesso!", type="message")
    }, error = function(e) { print(e$message);
      showNotification(paste("Erro na transformação:", e$message), type="error")
    })
  })
  
  observeEvent(input$btn_remove_row, {
    req(rv$data, input$tabela_dados_rows_selected)
    n_removed <- length(input$tabela_dados_rows_selected)
    if(n_removed > 0) {
      rv$data <- rv$data[-input$tabela_dados_rows_selected, ]
      showNotification(paste0(n_removed, " linhas removidas."), type="warning", duration=4)
    }
  })
  
  observeEvent(input$btn_reset_data, {
    req(rv$raw_data)
    rv$data <- rv$raw_data
    showNotification("Banco restaurado ao original.", type="message", duration=4)
  })
  
  output$tabela_dados <- renderDT({ 
    req(rv$data)
    datatable(rv$data, selection="multiple", options=list(pageLength=15, scrollX=TRUE), class="compact stripe hover") 
  })
  
  # ─── Descriptive Statistics Table ────────────────────────
  desc_stats <- reactive({
    req(rv$data)
    df <- rv$data
    stats_list <- lapply(names(df), function(col_name) {
      col <- df[[col_name]]
      n_na <- sum(is.na(col))
      if (is.numeric(col)) {
        col_clean <- col[!is.na(col)]
        media <- mean(col_clean)
        dp    <- sd(col_clean)
        cv    <- if (!is.na(media) && media != 0) round(abs(dp / media) * 100, 1) else NA
        data.frame(
          Coluna   = col_name,
          Tipo     = "Numérica",
          n        = length(col_clean),
          NAs      = n_na,
          Média    = round(media, 3),
          DP       = round(dp, 3),
          "CV%"    = cv,
          Mín      = round(min(col_clean), 3),
          Mediana  = round(median(col_clean), 3),
          Máx      = round(max(col_clean), 3),
          Moda     = NA_character_,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      } else {
        col_fac <- as.factor(col)
        tab  <- table(col_fac)
        moda <- if (length(tab) > 0) names(tab)[which.max(tab)] else NA_character_
        n_niveis <- length(levels(col_fac))
        data.frame(
          Coluna   = col_name,
          Tipo     = paste0("Fator (", n_niveis, " níveis)"),
          n        = sum(!is.na(col)),
          NAs      = n_na,
          Média    = NA_real_,
          DP       = NA_real_,
          "CV%"    = NA_real_,
          Mín      = NA_real_,
          Mediana  = NA_real_,
          Máx      = NA_real_,
          Moda     = moda,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      }
    })
    do.call(rbind, stats_list)
  })
  
  output$tabela_desc_stats <- renderDT({
    req(desc_stats())
    datatable(
      desc_stats(),
      options = list(pageLength = 20, scrollX = TRUE, dom = "t"),
      rownames = FALSE,
      class = "compact stripe hover"
    ) |>
      formatStyle(
        "NAs",
        backgroundColor = styleInterval(0, c("transparent", "#FFF3CD"))
      )
  })

  # ══════════════════════════════════════════════════════════
  #  TAB 1 - AED
  # ══════════════════════════════════════════════════════════
  output$plot_aed <- renderPlot({
    req(rv$data, rv$aed_resp, rv$aed_trat1)
    req(rv$aed_trat1 != "Nenhum")
    pal <- cur_palette(); fsize <- cur_fsize(); alpha_val <- cur_alpha(); leg_pos <- cur_legend()
    theme_fn <- cur_theme(); tt <- input$transf_aed %||% "Nenhuma"
    df <- rv$data
    ylabel <- transf_label(rv$aed_resp, tt)
    if (tt != "Nenhuma") { df[["y_transf"]] <- apply_transform(df[[rv$aed_resp]], tt) } else { df[["y_transf"]] <- df[[rv$aed_resp]] }
    df[[rv$aed_trat1]] <- as.factor(df[[rv$aed_trat1]])
    df <- apply_plot_levels(df, rv$aed_trat1, input$custom_levels_1, rv$aed_trat2, input$custom_levels_2)
    
    # ── Gráfico 1: Boxplot com Jitter
    p1 <- ggplot(df, aes(x=!!sym(rv$aed_trat1), y=y_transf, fill=!!sym(rv$aed_trat1)))
    if (!is.null(rv$aed_trat2) && rv$aed_trat2 != "Nenhum" && rv$aed_trat2 %in% names(df)) {
      p1 <- ggplot(df, aes(x=!!sym(rv$aed_trat1), y=y_transf, fill=!!sym(rv$aed_trat2)))
    }
    p1 <- add_geom_type(p1, cur_plot_type(), alpha_val)
    if (cur_jitter()) {
      p1 <- p1 + geom_point(position=position_jitterdodge(jitter.width=0.12, dodge.width=0.75), 
                            alpha=alpha_val*0.7, size=cur_pt_size(), color="gray30")
    }
    p1 <- p1 + scale_fill_manual(values=pal) +
      theme_fn(base_size=fsize) +
      theme(plot.title=element_text(face="bold")) +
      labs(title=paste("Distribuição:", rv$aed_resp), x=rv$aed_trat1, y=ylabel)
    p1 <- apply_custom_labels(p1, input)
    
    # ── Gráfico 2: Resumo (Média ± Erro Padrão)
    p2 <- ggplot(df, aes(x=!!sym(rv$aed_trat1), y=y_transf, color=!!sym(rv$aed_trat1)))
    if (!is.null(rv$aed_trat2) && rv$aed_trat2 != "Nenhum" && rv$aed_trat2 %in% names(df)) {
      p2 <- ggplot(df, aes(x=!!sym(rv$aed_trat1), y=y_transf, color=!!sym(rv$aed_trat2), group=!!sym(rv$aed_trat2)))
    }
    p2 <- p2 + stat_summary(fun.data = mean_se, geom = "pointrange", size = 1, linewidth = 1, position = position_dodge(width = 0.5))
    p2 <- p2 + scale_color_manual(values=pal) +
      theme_fn(base_size=fsize) +
      theme(plot.title=element_text(face="bold")) +
      labs(title="Média ± SE", x=rv$aed_trat1, y=ylabel)
    p2 <- apply_custom_labels(p2, input)

    # ── Facetamento (Se Fatorial)
    if (isTRUE(input$aed_use_facet) && !is.null(rv$aed_trat2) && rv$aed_trat2 != "Nenhum" && rv$aed_trat2 %in% names(df)) {
      p1 <- p1 + facet_wrap(as.formula(paste("~", rv$aed_trat2)))
      p2 <- p2 + facet_wrap(as.formula(paste("~", rv$aed_trat2)))
    }

    # ── Combinar com Patchwork
    if (isTRUE(input$aed_show_patchwork)) {
      p_final <- p1 + p2 + plot_layout(guides = "collect") & theme(legend.position = leg_pos)
      export$plot_aed <- p_final
      return(p_final)
    } else {
      export$plot_aed <- p1
      return(p1)
    }
  })
  
  output$dl_plot_aed     <- downloadHandler("AED_plot.pdf", function(f) ggsave(f, export$plot_aed, device="pdf", width=cur_pdf_w(), height=cur_pdf_h()))
  output$dl_plot_aed_png <- downloadHandler("AED_plot.png", function(f) ggsave(f, export$plot_aed, device="png", width=cur_pdf_w(), height=cur_pdf_h(), dpi=300))
  output$dl_plot_aed_svg <- downloadHandler("AED_plot.svg", function(f) ggsave(f, export$plot_aed, device="svg", width=cur_pdf_w(), height=cur_pdf_h()))

  # ── Modelo AED (reactive) ───────────────────────────────
  modelo_aed <- reactive({
    req(rv$data, rv$aed_trat1, rv$aed_resp)
    req(rv$aed_trat1 != "Nenhum")
    tt <- input$transf_aed %||% "Nenhuma"
    df <- rv$data
    if (tt != "Nenhuma") { df[["resp_transf"]] <- apply_transform(df[[rv$aed_resp]], tt) } else { df[["resp_transf"]] <- df[[rv$aed_resp]] }
    df[[rv$aed_trat1]] <- as.factor(df[[rv$aed_trat1]])
    ef <- rv$aed_trat1
    if (!is.null(rv$aed_trat2) && rv$aed_trat2 != "Nenhum" && rv$aed_trat2 %in% names(df)) {
      df[[rv$aed_trat2]] <- as.factor(df[[rv$aed_trat2]])
      ef <- paste(ef, "*", rv$aed_trat2)
    }
    if (!is.null(rv$aed_bloco) && rv$aed_bloco != "Nenhum" && rv$aed_bloco %in% names(df)) {
      df[[rv$aed_bloco]] <- as.factor(df[[rv$aed_bloco]])
      ef <- paste(ef, "+", rv$aed_bloco)
    }
    lm(as.formula(paste("resp_transf ~", ef)), data = df)
  })

  # ── DHARMa Residuals (reactive) ──
  residuos_dharma_aed <- reactive({
    req(modelo_aed())
    simulateResiduals(fittedModel = modelo_aed(), n = 250)
  })

  # ── Badge de Uniformidade dos Resíduos (DHARMa) ──
  output$ui_aed_uniformity_badge <- renderUI({
    req(rv$data, rv$aed_resp, rv$aed_trat1)
    if (is.null(rv$aed_trat1) || rv$aed_trat1 == "Nenhum") {
      return(tags$div(class="alert alert-info m-3", icon("circle-info"), " Selecione o Tratamento 1 e a Resposta."))
    }
    sim_res <- tryCatch(residuos_dharma_aed(), error = function(e) NULL)
    if (is.null(sim_res)) return(NULL)
    
    unif_ui <- tryCatch({
      unif_test <- testUniformity(sim_res, plot = FALSE)
      pval <- unif_test$p.value
      cls  <- if (pval > 0.05) "alert-success" else "alert-danger"
      icn  <- if (pval > 0.05) icon("circle-check") else icon("circle-xmark")
      msg  <- if (pval > 0.05)
        paste0("DHARMa (Uniformidade): KS = ", round(unif_test$statistic, 4), ", p = ", format(pval, digits=4), " — Distribuição correta assumida (p > 0.05).")
      else
        paste0("DHARMa (Uniformidade): KS = ", round(unif_test$statistic, 4), ", p = ", format(pval, digits=4), " — Desvio significativo (p ≤ 0.05).")
      tags$div(class=paste("alert", cls, "d-flex align-items-center gap-2 m-3"), icn, msg)
    }, error=function(e) tags$div(class="alert alert-warning m-3", icon("triangle-exclamation"), " Teste de Uniformidade: ", e$message))
    
    unif_ui
  })

  # ── Badges de Dispersão (DHARMa) ──
  output$ui_aed_dispersion_badge <- renderUI({
    req(rv$data, rv$aed_resp, rv$aed_trat1)
    if (is.null(rv$aed_trat1) || rv$aed_trat1 == "Nenhum") return(NULL)
    sim_res <- tryCatch(residuos_dharma_aed(), error = function(e) NULL)
    if (is.null(sim_res)) return(NULL)
    
    disp_ui <- tryCatch({
      disp_test <- testDispersion(sim_res, plot = FALSE)
      pval <- disp_test$p.value
      cls  <- if (pval > 0.05) "alert-success" else "alert-danger"
      icn  <- if (pval > 0.05) icon("circle-check") else icon("circle-xmark")
      msg  <- if (pval > 0.05) paste0("DHARMa (Dispersão): Razão = ", round(disp_test$statistic, 4), ", p = ", format(pval, digits=4), " — Dispersão adequada (p > 0.05).") else paste0("DHARMa (Dispersão): Razão = ", round(disp_test$statistic, 4), ", p = ", format(pval, digits=4), " — Problemas de dispersão detectados (p ≤ 0.05).")
      tags$div(class=paste("alert", cls, "d-flex align-items-center gap-2 mb-2"), icn, msg)
    }, error=function(e) tags$div(class="alert alert-warning mb-2", icon("triangle-exclamation"), " Teste de Dispersão: ", e$message))
    
    tags$div(class="p-3", disp_ui)
  })

  # ── Plots DHARMa
  output$plot_aed_qq_dharma <- renderPlot({
    req(residuos_dharma_aed())
    plotQQunif(residuos_dharma_aed())
  })
  
  output$plot_aed_resfit_dharma <- renderPlot({
    req(residuos_dharma_aed())
    plotResiduals(residuos_dharma_aed())
  })

  # ── Resultados brutos dos testes
  output$res_aed_pressupostos <- renderPrint({
    req(modelo_aed(), residuos_dharma_aed())
    m  <- modelo_aed()
    sim_res <- residuos_dharma_aed()
    
    delineamento <- if (!is.null(rv$aed_bloco) && rv$aed_bloco != "Nenhum") "DBC" else "DIC"
    
    cat("=== Modelo Ajustado (", delineamento, ") ===\n")
    print(summary(m))
    cat("\n=== DHARMa: Teste de Uniformidade ===\n")
    print(testUniformity(sim_res, plot = FALSE))
    cat("\n=== DHARMa: Teste de Dispersão ===\n")
    print(testDispersion(sim_res, plot = FALSE))
    cat("\n=== DHARMa: Teste de Outliers ===\n")
    print(testOutliers(sim_res, plot = FALSE))
  })

  # ══════════════════════════════════════════════════════════
  #  TAB 2 - TESTE T
  # ══════════════════════════════════════════════════════════
  ttest_data <- reactive({
    req(rv$data, rv$ttest_group, rv$ttest_resp)
    tt <- input$transf_ttest %||% "Nenhuma"
    df <- rv$data
    if (tt != "Nenhuma") {
      df[["resp_transf"]] <- apply_transform(df[[rv$ttest_resp]], tt)
    } else {
      df[["resp_transf"]] <- df[[rv$ttest_resp]]
    }
    df
  })
  
  # ── Shapiro-Wilk badge na sidebar ───────────────────────
  output$ui_ttest_shapiro_badge <- renderUI({
    req(ttest_data())
    df <- ttest_data()
    niveis <- unique(na.omit(df[[rv$ttest_group]]))
    if (length(niveis) != 2) {
      return(tags$div(class="alert alert-warning p-2", style="font-size:0.82rem;",
             icon("triangle-exclamation"), " Selecione um grupo com 2 níveis."))
    }
    f <- as.formula(paste("resp_transf ~", rv$ttest_group))
    m <- tryCatch(lm(f, data = df), error = function(e) NULL)
    
    crit <- input$ttest_norm_crit %||% "shapiro"
    pval <- 1
    if (!is.null(m)) {
      if (crit == "shapiro") {
        sh_res <- tryCatch(shapiro.test(resid(m)), error=function(e) list(p.value=1, statistic=NA))
        pval <- sh_res$p.value
        msg_ok <- "✓ Resíduos Normais (p > 0.05). Teste T recomendado."
        msg_fail <- "✗ Resíduos Não-normais (p < 0.05). Wilcoxon recomendado."
      } else if (crit == "tcl") {
        n_obs <- nrow(df)
        if (n_obs > 30) {
          pval <- 1
          msg_ok <- paste0("✓ N = ", n_obs, " (> 30). Pelo TCL, Teste T é recomendado.")
          msg_fail <- msg_ok
        } else {
          sh_res <- tryCatch(shapiro.test(resid(m)), error=function(e) list(p.value=1, statistic=NA))
          pval <- sh_res$p.value
          msg_ok <- "✓ N ≤ 30, mas Resíduos Normais (p > 0.05). Teste T recomendado."
          msg_fail <- "✗ N ≤ 30 e Resíduos Não-normais (p < 0.05). Wilcoxon recomendado."
        }
      } else if (crit == "dharma") {
        sim_res <- tryCatch(DHARMa::simulateResiduals(m, n=250), error=function(e) NULL)
        if(!is.null(sim_res)){
           unif <- DHARMa::testUniformity(sim_res, plot=FALSE)
           pval <- unif$p.value
        }
        msg_ok <- "✓ Resíduos Uniformes (DHARMa p > 0.05). Teste T recomendado."
        msg_fail <- "✗ Resíduos Não-Uniformes (DHARMa p < 0.05). Wilcoxon recomendado."
      }
    }
    
    if (pval > 0.05) {
      cls <- "alert-success"
      msg <- msg_ok
    } else {
      cls <- "alert-danger"
      msg <- msg_fail
    }
    tags$div(class=paste("alert", cls, "p-2"), style="font-size:0.82rem;", msg)
  })
  
  # ── Helper: decide qual teste usar ──────────────────────
  ttest_get_results <- reactive({
    req(ttest_data())
    df <- ttest_data()
    niveis <- unique(na.omit(df[[rv$ttest_group]]))
    req(length(niveis) == 2)
    f <- as.formula(paste("resp_transf ~", rv$ttest_group))
    # ── Teste de homocedasticidade (Levene) para var.equal do T-test ──
    lev_p <- tryCatch(
      car::leveneTest(f, data = df)[1, "Pr(>F)"],
      error = function(e) NA_real_
    )
    # var.equal=TRUE (Student) se variâncias iguais, FALSE (Welch) caso contrário
    var_eq   <- is.na(lev_p) || lev_p > 0.05
    t_res    <- t.test(f, data = df, var.equal = var_eq)
    t_welch  <- t.test(f, data = df, var.equal = FALSE)  # sempre disponível para exibir
    wt       <- wilcox.test(f, data = df)
    
    m <- lm(f, data=df)
    crit <- input$ttest_norm_crit %||% "shapiro"
    pval <- 1
    sh_test <- list(p.value=1, statistic=NA, method="Critério Manual/Forçado")
    
    if (crit == "shapiro") {
       sh_test <- tryCatch(shapiro.test(resid(m)), error=function(e) list(p.value=1, statistic=NA, method="Shapiro-Wilk"))
       pval <- sh_test$p.value
       crit_label <- "Shapiro-Wilk"
    } else if (crit == "tcl") {
       n_obs <- nrow(df)
       if (n_obs > 30) {
          pval <- 1
          sh_test <- list(p.value=1, statistic=NA, method=sprintf("Teorema Central do Limite (N = %d > 30)", n_obs))
          crit_label <- "TCL (N > 30)"
       } else {
          sh_test <- tryCatch(shapiro.test(resid(m)), error=function(e) list(p.value=1, statistic=NA, method="Shapiro-Wilk (N <= 30)"))
          pval <- sh_test$p.value
          crit_label <- "Shapiro-Wilk (N ≤ 30)"
       }
    } else if (crit == "dharma") {
       sim_res <- tryCatch(DHARMa::simulateResiduals(m, n=250), error=function(e) NULL)
       if(!is.null(sim_res)){
          sh_test <- DHARMa::testUniformity(sim_res, plot=FALSE)
          pval <- sh_test$p.value
       }
       crit_label <- "DHARMa Uniformidade"
    }
    
    sh <- list(p.value=pval, statistic=sh_test$statistic, test=sh_test, crit_label=crit_label,
               var_equal=var_eq, lev_p=lev_p)
    
    method <- input$ttest_method %||% "auto"
    chosen <- switch(method,
      "ttest"    = list(test=t_res,   label=if(var_eq) "Teste T (Student)" else "Teste T (Welch)", is_wilcox=FALSE),
      "wilcoxon" = list(test=wt,      label="Wilcoxon/Mann-Whitney", is_wilcox=TRUE),
      # auto
      if (pval > 0.05) list(test=t_res, label=if(var_eq) "Teste T — Student (auto)" else "Teste T — Welch (auto)", is_wilcox=FALSE)
      else             list(test=wt,    label="Wilcoxon/Mann-Whitney (auto)", is_wilcox=TRUE)
    )
    list(t_res=t_res, t_welch=t_welch, wt=wt, sh=sh, chosen=chosen)
  })
  
  output$plot_ttest <- renderPlot({
    req(ttest_data())
    df <- ttest_data()
    df <- apply_plot_levels(df, rv$ttest_group, input$custom_levels_1)
    pal <- cur_palette(); fsize <- cur_fsize(); alpha_val <- cur_alpha(); leg_pos <- cur_legend()
    theme_fn <- cur_theme(); tt <- input$transf_ttest %||% "Nenhuma"
    ylabel <- transf_label(rv$ttest_resp, tt)
    
    # Obter p-valor do teste escolhido para anotação
    res <- tryCatch(ttest_get_results(), error=function(e) NULL)
    p_label <- if (!is.null(res)) {
      pv <- res$chosen$test$p.value
      paste0(res$chosen$label, "\np = ", if(pv < 0.001) "< 0.001" else format(round(pv,3)))
    } else NULL
    
    p <- ggplot(df, aes(x=!!sym(rv$ttest_group), y=resp_transf, fill=!!sym(rv$ttest_group)))
    p <- add_geom_type(p, cur_plot_type(), alpha_val)
    if (cur_jitter()) p <- p + geom_jitter(width=.12, alpha=alpha_val*0.7, size=cur_pt_size())
    p <- p + scale_fill_manual(values=pal) +
      theme_fn(base_size=fsize) +
      theme(legend.position="none", plot.title=element_text(face="bold")) +
      labs(title="Comparação entre Grupos", x=rv$ttest_group, y=ylabel,
           subtitle=p_label)
    p <- apply_custom_labels(p, input)
    export$plot_ttest <- p; p
  })
  
  output$res_ttest <- renderPrint({
    req(ttest_data())
    df <- ttest_data()
    niveis <- unique(na.omit(df[[rv$ttest_group]])); if(length(niveis)!=2) return(cat("Requer 2 níveis."))
    tt <- input$transf_ttest %||% "Nenhuma"
    res <- ttest_get_results()
    out <- capture.output({
      if(tt!="Nenhuma") cat(paste0("*** Transformação aplicada: ", tt, " ***\n\n"))
      cat(paste0("=== Teste Selecionado: ", res$chosen$label, " ===\n"))
      print(res$chosen$test)
      
      cat(sprintf("\n=== Verificação de Normalidade: %s ===\n", res$sh$crit_label))
      if (!is.null(res$sh$test) && !is.null(res$sh$test$method)) {
         cat(paste0("Método: ", res$sh$test$method, "\n"))
         if (!is.na(res$sh$statistic)) cat(sprintf("Estatística: %.3f\n", res$sh$statistic))
         cat(sprintf("p-valor: %.4f\n", res$sh$p.value))
      } else {
         cat("Avaliação não disponível ou estatística não se aplica.\n")
      }
      
      cat("\n=== Teste de Homocedasticidade (Levene) ===\n")
      if (!is.na(res$sh$lev_p)) {
        cat(sprintf("p-valor Levene: %.4f — Variâncias %s (p %s 0.05)\n",
                    res$sh$lev_p,
                    if(res$sh$var_equal) "IGUAIS" else "DIFERENTES",
                    if(res$sh$var_equal) ">" else "≤"))
        cat(sprintf("Variante usada: %s\n", if(res$sh$var_equal) "Student (var.equal=TRUE)" else "Welch (var.equal=FALSE)"))
      } else {
        cat("Não foi possível calcular o Levene test.\n")
      }
      
      cat("\n=== Teste T (variante automática) ===\n"); print(res$t_res)
      cat("\n=== Teste T (Welch — referência) ===\n"); print(res$t_welch)
      cat("\n=== Wilcoxon (Mann-Whitney U) ===\n"); print(res$wt)
      
      cat("\n=== Tamanho de Efeito (Cohen's d) ===\n")
      tryCatch({
        cd <- effectsize::cohens_d(df[["resp_transf"]] ~ df[[rv$ttest_group]], pooled_sd=res$sh$var_equal)
        d_val <- cd$Cohens_d[1]
        cat(sprintf("d = %.3f\n", d_val))
        abs_d <- abs(d_val)
        interp <- if(abs_d < 0.2) "Negligenciável" else if(abs_d < 0.5) "Pequeno" else if(abs_d < 0.8) "Médio" else "Grande"
        cat(sprintf("Interpretação (Cohen, 1988): Efeito %s\n", interp))
      }, error=function(e) cat("Não foi possível calcular.\n"))
    })
    export$res_ttest <- out; cat(out, sep="\n")
    export$model_ttest <- modelo_ttest()
  })
  
  output$interp_ttest <- renderUI({
    req(ttest_data())
    df <- ttest_data()
    niveis <- unique(na.omit(df[[rv$ttest_group]])); if(length(niveis)!=2) return(tags$div(class="interp-box","Selecione um grupo com 2 níveis."))
    tt <- input$transf_ttest %||% "Nenhuma"
    res <- ttest_get_results()
    t_res <- res$t_res; wt <- res$wt; sh <- res$sh; chosen <- res$chosen
    medias <- df |> group_by(across(all_of(rv$ttest_group))) |> summarise(m=mean(resp_transf, na.rm=TRUE), .groups="drop")
    g1 <- medias[[rv$ttest_group]][1]; g2 <- medias[[rv$ttest_group]][2]
    m1 <- round(medias$m[1],2); m2 <- round(medias$m[2],2)
    
    normal_txt <- if(sh$p.value>0.05)
      paste0("A premissa de normalidade foi <b>atendida ou assumida</b> (Critério: ", sh$crit_label, ").")
    else
      paste0("A premissa de normalidade <b>NÃO foi atendida</b> (Critério: ", sh$crit_label, ", p < 0.05).")
    
    # Info sobre homocedasticidade
    homoced_txt <- if (!is.na(sh$lev_p)) {
      if (sh$var_equal)
        paste0("Variâncias <b>iguais</b> (Levene p = ", round(sh$lev_p, 3), " > 0.05) → Teste T de <b>Student</b> usado.")
      else
        paste0("Variâncias <b>diferentes</b> (Levene p = ", round(sh$lev_p, 3), " ≤ 0.05) → Teste T de <b>Welch</b> (correção automática) usado.")
    } else {
      "Levene test não calculado (número de grupos inválido)."
    }
    
    method_txt <- input$ttest_method %||% "auto"
    method_label <- switch(method_txt,
      "auto"     = paste0("<b>Automático</b> — ", chosen$label, " foi selecionado com base no Shapiro-Wilk."),
      "ttest"    = "<b>Teste T</b> forçado pelo usuário (paramétrico).",
      "wilcoxon" = "<b>Wilcoxon/Mann-Whitney</b> forçado pelo usuário (não-paramétrico)."
    )
    
    p_usado <- chosen$test$p.value
    concl <- if(p_usado<=0.05)
      paste0("Existe <b>diferença estatisticamente significativa</b> entre os grupos <b>",g1,"</b> (média=",m1,") e <b>",g2,"</b> (média=",m2,").")
    else
      paste0("<b>Não há</b> diferença estatisticamente significativa entre os grupos <b>",g1,"</b> (média=",m1,") e <b>",g2,"</b> (média=",m2,").")
    
    items <- list()
    if (tt != "Nenhuma") items <- c(items, list(tags$div(class="interp-item", HTML(paste0("<b>Transformação:</b> ", tt, " aplicada à variável resposta.")))))
    items <- c(items, list(
      tags$div(class="interp-item", HTML(paste0("<b>1. Normalidade:</b> ", normal_txt))),
      tags$div(class="interp-item", HTML(paste0("<b>2. Homocedasticidade:</b> ", homoced_txt))),
      tags$div(class="interp-item", HTML(paste0("<b>3. Critério do teste:</b> ", method_label))),
      tags$div(class="interp-item", HTML(paste0("<b>4. Significância:</b> ", badge_sig(p_usado)))),
      tags$div(class="interp-item", HTML(paste0("<b>5. Conclusão:</b> ", concl)))
    ))
    export$interp_ttest_html <- as.character(tagList(items))
    tags$div(class="interp-box", h5(icon("lightbulb")," Interpretação dos Resultados"), tagList(items))
  })
  
  output$sci_ttest <- renderUI({
    req(ttest_data())
    df <- ttest_data()
    niveis <- unique(na.omit(df[[rv$ttest_group]])); if(length(niveis)!=2) return(NULL)
    res <- ttest_get_results()
    t_res <- res$t_res; wt <- res$wt; sh <- res$sh; chosen <- res$chosen
    medias <- df |> group_by(across(all_of(rv$ttest_group))) |> summarise(m=mean(resp_transf, na.rm=TRUE), .groups="drop")
    g1 <- medias[[rv$ttest_group]][1]; g2 <- medias[[rv$ttest_group]][2]
    m1 <- round(medias$m[1],2); m2 <- round(medias$m[2],2)
    
    sh_txt <- if (sh$crit_label == "TCL (N > 30)") {
      "pelo Teorema Central do Limite (amostra > 30)"
    } else if (is.na(sh$statistic)) {
      sprintf("pelo critério %s (p = %.3f)", sh$crit_label, sh$p.value)
    } else {
      sprintf("pelo critério %s (Est = %.3f, p = %.3f)", sh$crit_label, sh$statistic, sh$p.value)
    }
    
    if (!chosen$is_wilcox) {
      txt <- sprintf("Os dados foram submetidos ao Teste T para amostras independentes, visto que a premissa de normalidade dos resíduos foi atendida %s. O teste indicou que %s diferença estatisticamente significativa entre os tratamentos %s (Média = %.2f) e %s (Média = %.2f) (t = %.2f, df = %.1f, p %s).",
                     sh_txt,
                     if(t_res$p.value <= 0.05) "houve" else "não houve",
                     g1, m1, g2, m2,
                     t_res$statistic, t_res$parameter,
                     if(t_res$p.value < 0.001) "< 0.001" else sprintf("= %.3f", t_res$p.value))
    } else {
      txt <- sprintf("Os dados foram submetidos ao Teste Não-Paramétrico de Wilcoxon-Mann-Whitney (U). Os resultados revelaram que %s diferença significativa entre os tratamentos %s (Média = %.2f) e %s (Média = %.2f) (W = %.1f, p %s). A premissa de normalidade não foi atendida %s.",
                     if(wt$p.value <= 0.05) "houve" else "não houve",
                     g1, m1, g2, m2,
                     wt$statistic,
                     if(wt$p.value < 0.001) "< 0.001" else sprintf("= %.3f", wt$p.value),
                     sh_txt)
    }
    
    export$txt_ttest <- txt
    tags$div(class="interp-box", h5(icon("quote-left")," Texto Sugerido para Publicação"), p(txt),
             tags$small(class="text-muted", paste0("Teste usado: ", chosen$label, " | Nota: Revise o texto antes de publicá-lo.")))
  })
  
  output$dl_plot_ttest     <- downloadHandler("ttest_plot.pdf", function(f) ggsave(f, export$plot_ttest, device="pdf", width=cur_pdf_w(), height=cur_pdf_h()))
  output$dl_plot_ttest_png <- downloadHandler("ttest_plot.png", function(f) ggsave(f, export$plot_ttest, device="png", width=cur_pdf_w(), height=cur_pdf_h(), dpi=300))
  output$dl_plot_ttest_svg <- downloadHandler("ttest_plot.svg", function(f) ggsave(f, export$plot_ttest, device="svg", width=cur_pdf_w(), height=cur_pdf_h()))
  output$dl_res_ttest  <- downloadHandler("ttest_res.txt",  function(f) writeLines(export$res_ttest, f))
  output$dl_report_ttest <- downloadHandler("ttest_report.txt", function(f) writeLines(export$txt_ttest %||% "", f))
  
  observeEvent(input$btn_copy_ttest, {
    req(export$res_ttest)
    tryCatch({
      clipr::write_clip(export$res_ttest)
      showNotification("Relatório completo copiado para a área de transferência!", type="message")
    }, error = function(e) { print(e$message);
      showNotification("Erro ao copiar. Verifique se o pacote 'clipr' está instalado e funcional.", type="error")
    })
  })
  
  # ══════════════════════════════════════════════════════════
  #  TAB 3 - ANOVA
  # ══════════════════════════════════════════════════════════
  modelo_anova <- reactive({
    req(rv$data, rv$anova_trat1, rv$anova_resp)
    tt <- input$transf_anova %||% "Nenhuma"
    df <- rv$data
    if (tt != "Nenhuma") { df[["resp_transf"]] <- apply_transform(df[[rv$anova_resp]], tt) } else { df[["resp_transf"]] <- df[[rv$anova_resp]] }
    df[[rv$anova_trat1]] <- as.factor(df[[rv$anova_trat1]]); ef <- rv$anova_trat1
    if (rv$anova_trat2 != "Nenhum") { df[[rv$anova_trat2]] <- as.factor(df[[rv$anova_trat2]]); ef <- paste(ef, "*", rv$anova_trat2) }
    
    is_rand <- isTRUE(input$anova_bloco_rand)
    if (rv$anova_bloco != "Nenhum") { 
      df[[rv$anova_bloco]] <- as.factor(df[[rv$anova_bloco]])
      if (is_rand) {
        ef <- paste(ef, "+ (1|", rv$anova_bloco, ")")
      } else {
        ef <- paste(ef, "+", rv$anova_bloco)
      }
    }
    
    fml <- as.formula(paste("resp_transf", "~", ef))
    if (is_rand && rv$anova_bloco != "Nenhum") {
      lmerTest::lmer(fml, data=df)
    } else {
      lm(fml, data=df)
    }
  })
  
  output$plot_anova_tukey <- renderPlot({
    req(modelo_anova())
    m <- modelo_anova(); df <- rv$data
    pal <- cur_palette(); fsize <- cur_fsize(); alpha_val <- cur_alpha(); leg_pos <- cur_legend()
    theme_fn <- cur_theme(); tt <- input$transf_anova %||% "Nenhuma"
    ylabel <- transf_label(rv$anova_resp, tt)
    
    if (input$anova_method != "Paramétrico (ANOVA)") {
      df_plot <- df; df_plot[[rv$anova_trat1]] <- as.factor(df_plot[[rv$anova_trat1]])
      if (tt != "Nenhuma") { df_plot[["resp_transf"]] <- apply_transform(df_plot[[rv$anova_resp]], tt) } else { df_plot[["resp_transf"]] <- df_plot[[rv$anova_resp]] }
      
      letras_df <- get_nonpar_letters(df_plot, "resp_transf", rv$anova_trat1)
      names(letras_df)[1] <- rv$anova_trat1
      
      letras_df <- apply_plot_levels(letras_df, rv$anova_trat1, input$custom_levels_1)
      df_plot <- apply_plot_levels(df_plot, rv$anova_trat1, input$custom_levels_1)
      
      max_y <- df_plot |> group_by(!!sym(rv$anova_trat1)) |> summarise(max_val = max(resp_transf, na.rm=TRUE))
      letras_df <- left_join(letras_df, max_y, by=rv$anova_trat1)
      
      p <- ggplot(df_plot, aes(x=!!sym(rv$anova_trat1), y=resp_transf, fill=!!sym(rv$anova_trat1)))
      p <- add_geom_type(p, cur_plot_type(), alpha_val)
      if (cur_jitter()) p <- p + geom_jitter(width=0.15, alpha=alpha_val*0.8, size=cur_pt_size(), color="gray30")
      p <- p + geom_text(data=letras_df, aes(x=!!sym(rv$anova_trat1), y=max_val, label=.group), vjust=-0.8, fontface="bold", size=5) +
        scale_fill_manual(values=pal) + scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) + theme_fn(base_size=fsize) +
        theme(plot.title=element_text(face="bold"), legend.position="none") +
        labs(title="Boxplot e Teste de Wilcoxon (Holm)", y=paste("Mediana —", ylabel))
      p <- apply_custom_labels(p, input)
      export$plot_anova <- p; return(p)
    }
    
    df_plot <- df
    if (tt != "Nenhuma") { df_plot[["resp_transf"]] <- apply_transform(df_plot[[rv$anova_resp]], tt) } else { df_plot[["resp_transf"]] <- df_plot[[rv$anova_resp]] }
    df_plot[[rv$anova_trat1]] <- as.factor(df_plot[[rv$anova_trat1]])

    if (rv$anova_trat2 != "Nenhum") {
      df_plot[[rv$anova_trat2]] <- as.factor(df_plot[[rv$anova_trat2]])
      df_plot <- apply_plot_levels(df_plot, rv$anova_trat1, input$custom_levels_1, rv$anova_trat2, input$custom_levels_2)
      
      letras_df <- clean_cld(emmeans(m, as.formula(paste("~", rv$anova_trat1, "|", rv$anova_trat2))))
      letras_df <- apply_plot_levels(letras_df, rv$anova_trat1, input$custom_levels_1, rv$anova_trat2, input$custom_levels_2)
      
      max_y <- df_plot |> group_by(!!sym(rv$anova_trat1), !!sym(rv$anova_trat2)) |> summarise(max_val = max(resp_transf, na.rm=TRUE))
      letras_df <- left_join(letras_df, max_y, by=c(rv$anova_trat1, rv$anova_trat2))

      p <- ggplot(df_plot, aes(x=!!sym(rv$anova_trat1), y=resp_transf, fill=!!sym(rv$anova_trat2)))
      p <- add_geom_type(p, cur_plot_type(), alpha_val)
      if (cur_jitter()) {
        p <- p + geom_point(position=position_jitterdodge(jitter.width=0.15, dodge.width=0.75), alpha=alpha_val*0.8, size=cur_pt_size(), color="gray30")
      }
      p <- p + geom_text(data=letras_df, aes(x=!!sym(rv$anova_trat1), y=max_val, label=.group, group=!!sym(rv$anova_trat2)), position=position_dodge(0.75), vjust=-0.8, fontface="bold", size=5) +
        scale_fill_manual(values=pal) + scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
        theme_fn(base_size=fsize) +
        theme(plot.title=element_text(face="bold"), legend.position=leg_pos) +
        labs(y=ylabel, title="Interação Fatorial (Tukey)")
    } else {
      df_plot <- apply_plot_levels(df_plot, rv$anova_trat1, input$custom_levels_1)
      
      letras_df <- clean_cld(emmeans(m, as.formula(paste("~", rv$anova_trat1))))
      letras_df <- apply_plot_levels(letras_df, rv$anova_trat1, input$custom_levels_1)
      
      max_y <- df_plot |> group_by(!!sym(rv$anova_trat1)) |> summarise(max_val = max(resp_transf, na.rm=TRUE))
      letras_df <- left_join(letras_df, max_y, by=rv$anova_trat1)
      
      p <- ggplot(df_plot, aes(x=!!sym(rv$anova_trat1), y=resp_transf, fill=!!sym(rv$anova_trat1)))
      p <- add_geom_type(p, cur_plot_type(), alpha_val)
      if (cur_jitter()) {
        p <- p + geom_jitter(width=0.15, alpha=alpha_val*0.8, size=cur_pt_size(), color="gray30")
      }
      p <- p + geom_text(data=letras_df, aes(x=!!sym(rv$anova_trat1), y=max_val, label=.group), vjust=-0.8, fontface="bold", size=5) +
        scale_fill_manual(values=pal) + scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
        theme_fn(base_size=fsize) +
        theme(plot.title=element_text(face="bold"), legend.position="none") +
        labs(title="Médias e Grupos (Tukey)", y=ylabel)
    }
    p <- apply_custom_labels(p, input)
    export$plot_anova <- p; p
  })
  
  output$res_anova <- renderPrint({
    req(modelo_anova())
    tt <- input$transf_anova %||% "Nenhuma"
    df <- rv$data
    if (tt != "Nenhuma") { df[["resp_transf"]] <- apply_transform(df[[rv$anova_resp]], tt) } else { df[["resp_transf"]] <- df[[rv$anova_resp]] }
    df[[rv$anova_trat1]] <- as.factor(df[[rv$anova_trat1]])
    
    out <- capture.output({
      if(tt!="Nenhuma") cat(paste0("*** Transformação aplicada: ", tt, " ***\n\n"))
      
      if (input$anova_method == "Paramétrico (ANOVA)") {
        cat("=== Quadro da ANOVA ===\n"); print(anova(modelo_anova()))
        
        cat("\n=== Tamanho de Efeito (Eta-quadrado) ===\n")
        tryCatch({
          eta <- effectsize::eta_squared(modelo_anova(), partial = FALSE)
          print(eta)
          cat("Interpretação: Proporção da variância total explicada pelo fator.\n")
        }, error=function(e) cat("Não foi possível calcular.\n"))
        
        adj_method <- switch(input$anova_posthoc %||% "Tukey", "Tukey"="tukey", "Fisher (LSD)"="none", "Bonferroni"="bonferroni")
        cat(paste0("\n=== Comparações Múltiplas (", input$anova_posthoc %||% "Tukey", ") ===\n"))
        if (rv$anova_trat2 != "Nenhum") {
          print(cld(emmeans(modelo_anova(), as.formula(paste("~", rv$anova_trat1, "|", rv$anova_trat2))), Letters=letters, adjust=adj_method))
        } else {
          print(cld(emmeans(modelo_anova(), as.formula(paste("~", rv$anova_trat1))), Letters=letters, adjust=adj_method))
        }
      } else {
        cat("=== Teste Não-Paramétrico ===\n")
        if (rv$anova_trat2 != "Nenhum") cat("Aviso: Testes não-paramétricos fatoriais complexos não são totalmente suportados. Avaliando apenas o Fator 1.\n\n")
        
        if (rv$anova_bloco != "Nenhum") {
          cat("Teste de Friedman (DBC):\n")
          df[[rv$anova_bloco]] <- as.factor(df[[rv$anova_bloco]])
          ft <- suppressWarnings(friedman.test(
            df[["resp_transf"]],
            as.factor(df[[rv$anova_trat1]]),  # garantir fator
            df[[rv$anova_bloco]]
          ))
          print(ft)
        } else {
          cat("Teste de Kruskal-Wallis (DIC):\n")
          kt <- kruskal.test(
            df[["resp_transf"]],
            as.factor(df[[rv$anova_trat1]])   # garantir fator
          )
          print(kt)
        }
        cat("\n=== Comparações Múltiplas (Wilcoxon pareado com ajuste Holm) ===\n")
        print(suppressWarnings(pairwise.wilcox.test(
          df[["resp_transf"]],
          as.factor(df[[rv$anova_trat1]]),    # garantir fator
          p.adjust.method="holm", exact=FALSE
        )))
      }
    })
    export$res_anova <- out; cat(out, sep="\n")
    export$model_anova <- modelo_anova()
  })
  
  output$plot_anova_dharma <- renderPlot({ 
    if(input$anova_method != "Paramétrico (ANOVA)") return(plot(1, type="n", axes=FALSE, xlab="", ylab="", main="DHARMa não se aplica a testes não-paramétricos."))
    req(modelo_anova())
    res <- simulateResiduals(modelo_anova())
    export$dharma_anova <- res
    plot(res)
  })
  
  output$interp_anova <- renderUI({
    req(modelo_anova())
    if (input$anova_method != "Paramétrico (ANOVA)") return(tags$div(class="interp-box", h5(icon("lightbulb")," Interpretação Não-Paramétrica"), HTML("Você selecionou o teste não-paramétrico. Acesse a aba <b>Texto Científico</b> para uma descrição dos resultados e veja o gráfico com o agrupamento do teste de Wilcoxon.")))
    
    m <- modelo_anova(); av <- anova(m)
    tt <- input$transf_anova %||% "Nenhuma"
    
    is_rand <- inherits(m, "lmerMod")
    if (is_rand) {
      delineamento <- "Misto (DBC com Bloco Aleatório)"
    } else {
      delineamento <- if(rv$anova_bloco=="Nenhum") "DIC (Delineamento Inteiramente Casualizado)" else "DBC (Delineamento em Blocos Casualizados)"
    }
    items <- list()
    if (tt != "Nenhuma") items <- c(items, list(tags$div(class="interp-item", HTML(paste0("<b>Transformação:</b> ", tt, " aplicada à variável resposta.")))))
    items <- c(items, list(tags$div(class="interp-item", HTML(paste0("<b>Delineamento:</b> ", delineamento)))))
    termos <- rownames(av); termos <- termos[termos!="Residuals"]
    for(t in termos){
      pv <- av[t,"Pr(>F)"]; lbl <- t
      if(grepl(":",t)) lbl <- paste0("Interação (",gsub(":"," × ",t),")")
      txt <- if(!is.na(pv)&&pv<=0.05) paste0("O fator <b>",lbl,"</b> teve efeito <b>significativo</b> sobre a resposta. ") else paste0("O fator <b>",lbl,"</b> <b>não</b> teve efeito significativo. ")
      items <- c(items, list(tags$div(class="interp-item", HTML(paste0(txt, badge_sig(pv))))))
    }
    has_inter <- any(grepl(":",termos)); p_inter <- if(has_inter) av[grep(":",termos,value=TRUE)[1],"Pr(>F)"] else NA
    if(has_inter && !is.na(p_inter) && p_inter<=0.05) items <- c(items, list(tags$div(class="interp-item",HTML("<b>Conclusão:</b> Como a interação é significativa, os efeitos dos fatores dependem um do outro. Interprete as médias pelo <b>desdobramento</b> (letras no gráfico)."))))
    else if(has_inter) items <- c(items, list(tags$div(class="interp-item",HTML("<b>Conclusão:</b> Interação não significativa. Interprete os efeitos principais de cada fator isoladamente."))))
    export$interp_anova_html <- as.character(tagList(items))
    tags$div(class="interp-box", h5(icon("lightbulb")," Interpretação da ANOVA"), tagList(items))
  })
  
  output$sci_anova <- renderUI({
    req(rv$data, rv$anova_trat1, rv$anova_resp)
    df <- rv$data
    tt <- input$transf_anova %||% "Nenhuma"
    if (tt != "Nenhuma") { df[["resp_transf"]] <- apply_transform(df[[rv$anova_resp]], tt) } else { df[["resp_transf"]] <- df[[rv$anova_resp]] }
    
    if (input$anova_method == "Paramétrico (ANOVA)") {
      m <- modelo_anova(); av <- anova(m)
      pv <- av[rv$anova_trat1, "Pr(>F)"]
      fval <- av[rv$anova_trat1, "F value"]
      if (inherits(m, "lmerMod")) {
        df1 <- round(av[rv$anova_trat1, "NumDF"], 1); df2 <- round(av[rv$anova_trat1, "DenDF"], 1)
        modelo_str <- "Modelo Linear Misto (Efeitos Aleatórios)"
      } else {
        df1 <- av[rv$anova_trat1, "Df"]; df2 <- av["Residuals", "Df"]
        modelo_str <- "Análise de Variância (ANOVA clássica)"
      }
      txt <- sprintf("Os dados foram submetidos a um %s. O efeito do tratamento principal sobre a variável resposta foi %s (F(%s, %s) = %.2f, p %s). %s",
                     modelo_str,
                     if(!is.na(pv) && pv <= 0.05) "estatisticamente significativo" else "não significativo",
                     df1, df2, fval,
                     if(!is.na(pv) && pv < 0.001) "< 0.001" else sprintf("= %.3f", pv),
                     if(!is.na(pv) && pv <= 0.05) paste0("As médias estimadas foram separadas pelo teste de ", input$anova_posthoc %||% "Tukey", " (p < 0.05).") else "")
    } else {
      if (rv$anova_bloco != "Nenhum") {
        ft <- suppressWarnings(friedman.test(df[["resp_transf"]], as.factor(df[[rv$anova_trat1]]), as.factor(df[[rv$anova_bloco]])))
        pv <- ft$p.value; chi <- ft$statistic; df1 <- ft$parameter
        txt <- sprintf("Devido ao não atendimento das premissas da ANOVA, os dados foram avaliados através do teste não-paramétrico de Friedman para blocos casualizados. Observou-se que o efeito do tratamento foi %s (\u03c7\u00b2(%d) = %.2f, p %s). %s",
                       if(pv <= 0.05) "estatisticamente significativo" else "não significativo",
                       df1, chi, if(pv < 0.001) "< 0.001" else sprintf("= %.3f", pv),
                       if(pv <= 0.05) "As comparações múltiplas foram realizadas pelo teste de Wilcoxon com correção de Holm (p < 0.05)." else "")
      } else {
        kt <- kruskal.test(df[["resp_transf"]], as.factor(df[[rv$anova_trat1]]))
        pv <- kt$p.value; chi <- kt$statistic; df1 <- kt$parameter
        txt <- sprintf("Devido ao não atendimento das premissas da ANOVA, os dados foram avaliados através do teste não-paramétrico de Kruskal-Wallis. Observou-se que o efeito do tratamento foi %s (\u03c7\u00b2(%d) = %.2f, p %s). %s",
                       if(pv <= 0.05) "estatisticamente significativo" else "não significativo",
                       df1, chi, if(pv < 0.001) "< 0.001" else sprintf("= %.3f", pv),
                       if(pv <= 0.05) "As comparações múltiplas foram realizadas pelo teste de Wilcoxon com correção de Holm (p < 0.05)." else "")
      }
    }
    
    export$txt_anova <- txt
    tags$div(class="interp-box", h5(icon("quote-left")," Texto Sugerido para Publicação"), p(txt),
             tags$small(class="text-muted", "Nota: Revise o texto antes de publicá-lo."))
  })
  
  output$dl_plot_anova     <- downloadHandler("anova_plot.pdf", function(f) ggsave(f, export$plot_anova, device="pdf", width=cur_pdf_w(), height=cur_pdf_h()))
  output$dl_plot_anova_png <- downloadHandler("anova_plot.png", function(f) ggsave(f, export$plot_anova, device="png", width=cur_pdf_w(), height=cur_pdf_h(), dpi=300))
  output$dl_plot_anova_svg <- downloadHandler("anova_plot.svg", function(f) ggsave(f, export$plot_anova, device="svg", width=cur_pdf_w(), height=cur_pdf_h()))
  output$dl_res_anova  <- downloadHandler("anova_res.txt",  function(f) writeLines(export$res_anova, f))
  output$dl_report_anova <- downloadHandler("anova_report.txt", function(f) writeLines(export$txt_anova, f))
  
  output$ui_tbl_fatorial_anova <- renderUI({
    req(rv$anova_trat2 != "Nenhum")
    tags$div(class="res-card mt-3", style="text-align: center; overflow-x: auto;",
      tags$h5(icon("table"), " Tabela de Interação Fatorial (Letras Maiúsculas e Minúsculas)"),
      tags$p(tags$small("As letras minúsculas comparam os níveis do Tratamento 1 dentro de cada nível do Tratamento 2 (colunas). As letras maiúsculas comparam os níveis do Tratamento 2 dentro de cada nível do Tratamento 1 (linhas).")),
      tags$div(class="d-flex justify-content-center", style="width: 100%; margin-top: 15px;",
        tableOutput("tbl_fatorial_anova")
      )
    )
  })
  
  output$tbl_fatorial_anova <- renderTable({
    req(modelo_anova(), rv$anova_trat2 != "Nenhum")
    # Gerar a tabela usando a nova helper function
    tab <- generate_factorial_table(modelo_anova(), rv$anova_trat1, rv$anova_trat2)
    export$tbl_anova_fat <- tab
    tab
  }, striped = TRUE, bordered = TRUE, hover = TRUE, na = "", sanitize.text.function = identity, width = "100%", align = "c")
  
  observeEvent(input$btn_copy_anova, {
    req(export$res_anova)
    tryCatch({
      clipr::write_clip(export$res_anova)
      showNotification("Relatório completo copiado para a área de transferência!", type="message")
    }, error = function(e) { print(e$message);
      showNotification("Erro ao copiar. Verifique se o pacote 'clipr' está instalado e funcional.", type="error")
    })
  })
  
  # ══════════════════════════════════════════════════════════
  #  TAB 4 - GLM (Poisson / Quasipoisson / Binomial Negativa)
  # ══════════════════════════════════════════════════════════
  glm_family_label <- reactive({
    fam <- input$glm_family %||% "Poisson"
    switch(fam, "Poisson"="Poisson", "Quasipoisson"="Quasipoisson", "Binomial Negativa"="Binomial Negativa", "Poisson")
  })
  
  modelo_glm <- reactive({
    req(rv$data, rv$glm_trat, rv$glm_resp)
    df <- rv$data
    df[[rv$glm_trat]] <- as.factor(df[[rv$glm_trat]])
    
    ef <- rv$glm_trat
    if (rv$glm_trat2 != "Nenhum" && rv$glm_trat2 %in% names(df)) {
      df[[rv$glm_trat2]] <- as.factor(df[[rv$glm_trat2]])
      ef <- paste(ef, "*", rv$glm_trat2)
    }
    
    fml_str <- paste(rv$glm_resp, "~", ef)
    
    if (rv$glm_offset != "Nenhum" && rv$glm_offset %in% names(df)) {
      fml_str <- paste(fml_str, "+ offset(log(", rv$glm_offset, "))")
    }
    
    fml <- as.formula(fml_str)
    fam <- input$glm_family %||% "Poisson"
    
    tryCatch({
      if (fam == "Binomial Negativa") {
        MASS::glm.nb(fml, data=df)
      } else if (fam == "Quasipoisson") {
        glm(fml, family=quasipoisson, data=df)
      } else {
        glm(fml, family=poisson, data=df)
      }
    }, error=function(e) {
      showNotification(paste("Erro no GLM:", e$message), type="error", duration=6)
      NULL
    })
  })
  
  glm_dispersion <- reactive({
    req(modelo_glm())
    m <- modelo_glm()
    pearson_chi2 <- sum(residuals(m, type="pearson")^2)
    df_res <- df.residual(m)
    ratio <- pearson_chi2 / df_res
    list(chi2=pearson_chi2, df=df_res, ratio=ratio)
  })
  
  output$plot_glm_medias <- renderPlot({
    req(modelo_glm())
    m <- modelo_glm()
    pal <- cur_palette(); fsize <- cur_fsize(); leg_pos <- cur_legend()
    theme_fn <- cur_theme(); alpha_val <- cur_alpha()
    fam_label <- glm_family_label()
    
    is_nb <- inherits(m, "negbin")
    resp_type <- "response"  # emmeans back-transforma para escala original
    
    df_plot <- rv$data
    df_plot[[rv$glm_trat]] <- as.factor(df_plot[[rv$glm_trat]])
    
    if (rv$glm_trat2 != "Nenhum" && rv$glm_trat2 %in% names(df_plot)) {
      df_plot[[rv$glm_trat2]] <- as.factor(df_plot[[rv$glm_trat2]])
      df_plot <- apply_plot_levels(df_plot, rv$glm_trat, input$custom_levels_1, rv$glm_trat2, input$custom_levels_2)
      
      em_fml <- as.formula(paste("~", rv$glm_trat, "|", rv$glm_trat2))
      letras_df <- clean_cld(emmeans(m, em_fml, type=resp_type))
      letras_df <- apply_plot_levels(letras_df, rv$glm_trat, input$custom_levels_1, rv$glm_trat2, input$custom_levels_2)
      
      max_y <- df_plot |> group_by(!!sym(rv$glm_trat), !!sym(rv$glm_trat2)) |> summarise(max_val = max(!!sym(rv$glm_resp), na.rm=TRUE))
      letras_df <- left_join(letras_df, max_y, by=c(rv$glm_trat, rv$glm_trat2))
      
      p <- ggplot(df_plot, aes(x=!!sym(rv$glm_trat), y=!!sym(rv$glm_resp), fill=!!sym(rv$glm_trat2)))
      p <- add_geom_type(p, cur_plot_type(), alpha_val)
      if (cur_jitter()) {
        p <- p + geom_point(position=position_jitterdodge(jitter.width=0.15, dodge.width=0.75), alpha=alpha_val*0.8, size=cur_pt_size(), color="gray30")
      }
      p <- p + geom_text(data=letras_df, aes(x=!!sym(rv$glm_trat), y=max_val, label=.group, group=!!sym(rv$glm_trat2)), position=position_dodge(0.75), vjust=-0.8, fontface="bold", size=5) +
        scale_fill_manual(values=pal) + scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
        theme_fn(base_size=fsize) +
        theme(plot.title=element_text(face="bold"), legend.position=leg_pos) +
        labs(y=paste("Resposta —", fam_label), title=paste("Interação Fatorial — GLM", fam_label))
        
    } else {
      df_plot <- apply_plot_levels(df_plot, rv$glm_trat, input$custom_levels_1)
      
      em_fml <- as.formula(paste("~", rv$glm_trat))
      letras_df <- clean_cld(emmeans(m, em_fml, type=resp_type))
      letras_df <- apply_plot_levels(letras_df, rv$glm_trat, input$custom_levels_1)
      
      max_y <- df_plot |> group_by(!!sym(rv$glm_trat)) |> summarise(max_val = max(!!sym(rv$glm_resp), na.rm=TRUE))
      letras_df <- left_join(letras_df, max_y, by=rv$glm_trat)
      
      p <- ggplot(df_plot, aes(x=!!sym(rv$glm_trat), y=!!sym(rv$glm_resp), fill=!!sym(rv$glm_trat)))
      p <- add_geom_type(p, cur_plot_type(), alpha_val)
      if (cur_jitter()) {
        p <- p + geom_jitter(width=0.15, alpha=alpha_val*0.8, size=cur_pt_size(), color="gray30")
      }
      p <- p + geom_text(data=letras_df, aes(x=!!sym(rv$glm_trat), y=max_val, label=.group), vjust=-0.8, fontface="bold", size=5) +
        scale_fill_manual(values=pal) + scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
        theme_fn(base_size=fsize) +
        theme(plot.title=element_text(face="bold"), legend.position="none") +
        labs(x="Tratamento", y=paste("Resposta —", fam_label), title=paste("Predições e Testes — GLM", fam_label))
    }
    p <- apply_custom_labels(p, input)
    export$plot_glm <- p; p
  })
  
  output$res_glm <- renderPrint({
    req(modelo_glm())
    m <- modelo_glm()
    disp <- glm_dispersion()
    fam_label <- glm_family_label()
    out <- capture.output({
      cat(paste0("=== GLM --- Fam\u00edlia: ", fam_label, " ===", "\n\n"))
      cat("=== Summary ===\n"); print(summary(m))
      if (!inherits(m, "negbin")) {
        cat("\n=== ANOVA (Deviance) ===\n"); print(anova(m, test="Chisq"))
      } else {
        cat("\n=== ANOVA (LRT) ===\n"); print(anova(m))
      }
      cat("\n=== Diagn\u00f3stico de Sobredispers\u00e3o ===\n")
      cat(sprintf("  Pearson Chi2 = %.2f / df = %d\n", disp$chi2, disp$df))
      cat(sprintf("  Raz\u00e3o de Dispers\u00e3o = %.3f\n", disp$ratio))
      if (disp$ratio > 1.5) cat("  >>> ATEN\u00c7\u00c3O: Poss\u00edvel sobredispers\u00e3o! Considere Quasipoisson ou Binomial Negativa.\n")
      else cat("  >>> Dispers\u00e3o adequada.\n")
      if (!inherits(m, "negbin") && !(is.list(m$family) && m$family$family == "quasipoisson")) {
        cat(sprintf("\n  AIC = %.2f\n", AIC(m)))
      }
    })
    export$res_glm <- out; cat(out, sep="\n")
    export$model_glm <- modelo_glm()
  })
  
  output$plot_glm_dharma <- renderPlot({
    req(modelo_glm())
    m <- modelo_glm()
    sim_res <- if (is.list(m$family) && m$family$family == "quasipoisson") {
      m_pois <- glm(formula(m), family=poisson, data=m$data)
      simulateResiduals(m_pois)
    } else {
      simulateResiduals(m)
    }
    plot(sim_res)
    
    # Teste de zero-inflation
    zi <- tryCatch(testZeroInflation(sim_res, plot = FALSE), error = function(e) NULL)
    if (!is.null(zi)) {
      zi_status <- if (zi$p.value < 0.05)
        sprintf("\nAVISO: Zero-inflation detectada! (p = %.4f) \u2014 Considere um modelo ZIP/ZINB.", zi$p.value)
      else
        sprintf("\nZero-inflation: n\u00e3o detectada (p = %.4f).", zi$p.value)
      message(zi_status)  # aparece no console / log
      mtext(zi_status, side = 1, line = 4, cex = 0.85,
            col = if(zi$p.value < 0.05) "red" else "darkgreen")
    }
  })
  
  output$interp_glm <- renderUI({
    req(modelo_glm()); m <- modelo_glm()
    fam_label <- glm_family_label()
    disp <- glm_dispersion()
    
    is_nb <- inherits(m, "negbin")
    em_fml <- as.formula(paste("~", rv$glm_trat))
    em_df <- as.data.frame(emmeans(m, em_fml, type="response"))
    
    rate_col <- if("rate" %in% names(em_df)) "rate" else if("response" %in% names(em_df)) "response" else "emmean"
    melhor <- em_df[which.min(em_df[[rate_col]]), rv$glm_trat]
    pior   <- em_df[which.max(em_df[[rate_col]]), rv$glm_trat]
    taxa_melhor <- round(min(em_df[[rate_col]]),2)
    taxa_pior   <- round(max(em_df[[rate_col]]),2)
    
    disp_status <- if(disp$ratio > 2) {
      paste0("<span class='badge bg-danger'>Sobredispers\u00e3o severa</span> (raz\u00e3o = ", round(disp$ratio,2), "). Recomenda-se <b>Binomial Negativa</b> ou <b>Quasipoisson</b>.")
    } else if(disp$ratio > 1.5) {
      paste0("<span class='badge bg-warning text-dark'>Sobredispers\u00e3o moderada</span> (raz\u00e3o = ", round(disp$ratio,2), "). Considere <b>Quasipoisson</b>.")
    } else if(disp$ratio < 0.5) {
      paste0("<span class='badge bg-info'>Subdispers\u00e3o</span> (raz\u00e3o = ", round(disp$ratio,2), "). Os erros-padr\u00e3o podem estar superestimados.")
    } else {
      paste0("<span class='badge bg-success'>Dispers\u00e3o adequada</span> (raz\u00e3o = ", round(disp$ratio,2), "). O modelo ", fam_label, " \u00e9 apropriado.")
    }
    
    items <- list(
      tags$div(class="interp-item", HTML(paste0("<b>Fam\u00edlia:</b> ", fam_label, " \u2014 fun\u00e7\u00e3o de liga\u00e7\u00e3o logar\u00edtmica."))),
      tags$div(class="interp-item", HTML(paste0("<b>Dispers\u00e3o:</b> ", disp_status))),
      tags$div(class="interp-item", HTML(paste0("<b>Menor contagem estimada:</b> O tratamento <b>", melhor, "</b> apresentou a menor taxa (", taxa_melhor, ")."))),
      tags$div(class="interp-item", HTML(paste0("<b>Maior contagem estimada:</b> O tratamento <b>", pior, "</b> apresentou a maior taxa (", taxa_pior, ").")))
    )
    
    if (rv$glm_trat2 != "Nenhum" && rv$glm_trat2 %in% names(rv$data)) {
      items <- c(items, list(tags$div(class="interp-item", HTML(paste0("<b>Fatorial:</b> O modelo inclui a intera\u00e7\u00e3o <b>", rv$glm_trat, " x ", rv$glm_trat2, "</b>. As letras no gr\u00e1fico comparam tratamentos dentro de cada n\u00edvel do segundo fator.")))))
    }
    if (rv$glm_offset != "Nenhum" && rv$glm_offset %in% names(rv$data)) {
      items <- c(items, list(tags$div(class="interp-item", HTML(paste0("<b>Offset:</b> A vari\u00e1vel <b>", rv$glm_offset, "</b> foi usada como offset (log). As taxas estimadas representam contagens <b>por unidade</b> de offset.")))))
    }
    
    items <- c(items, list(tags$div(class="interp-item", HTML("As letras no gráfico indicam quais tratamentos são <b>estatisticamente diferentes</b> entre si pelo teste de Tukey. Tratamentos com a <b>mesma letra</b> não diferem significativamente (p>0.05)."))))
    
    export$interp_glm_html <- as.character(tagList(items))
    tags$div(class="interp-box",
      h5(icon("lightbulb"), paste(" Interpretação do GLM", fam_label)),
      tagList(items)
    )
  })
  
  output$sci_glm <- renderUI({
    req(modelo_glm())
    m <- modelo_glm()
    fam_label <- glm_family_label()
    disp <- glm_dispersion()
    is_nb <- inherits(m, "negbin")
    
    # Extrair valores da ANOVA de forma segura para todas as familias
    pv <- NA_real_; stat_val <- NA_real_; df1 <- NA_integer_; test_name <- "deviance"
    tryCatch({
      if (is_nb) {
        av <- anova(m)
        av_df <- as.data.frame(av)
        test_name <- "LRT"
        # Encontrar a linha do tratamento
        trat_row <- which(rownames(av_df) == rv$glm_trat)
        if (length(trat_row) == 1) {
          # Buscar colunas por nome parcial (robustez)
          p_cols <- grep("Pr", names(av_df), value=TRUE)
          lr_cols <- grep("LR|stat", names(av_df), value=TRUE)
          df_cols <- grep("^Df$", names(av_df), value=TRUE)
          if (length(p_cols) > 0) pv <- av_df[trat_row, p_cols[1]]
          if (length(lr_cols) > 0) stat_val <- av_df[trat_row, lr_cols[1]]
          if (length(df_cols) > 0) df1 <- av_df[trat_row, df_cols[1]]
        }
      } else {
        av <- anova(m, test="Chisq")
        av_df <- as.data.frame(av)
        test_name <- "deviance"
        trat_row <- which(rownames(av_df) == rv$glm_trat)
        if (length(trat_row) == 1) {
          p_cols <- grep("Pr", names(av_df), value=TRUE)
          dev_cols <- grep("Deviance", names(av_df), value=TRUE)
          df_cols <- grep("^Df$", names(av_df), value=TRUE)
          if (length(p_cols) > 0) pv <- av_df[trat_row, p_cols[1]]
          if (length(dev_cols) > 0) stat_val <- av_df[trat_row, dev_cols[1]]
          if (length(df_cols) > 0) df1 <- av_df[trat_row, df_cols[1]]
        }
      }
    }, error = function(e) { print(e$message); })
    
    # Garantir que pv, stat_val, df1 sao escalares NA se NULL
    if (is.null(pv) || length(pv) == 0) pv <- NA_real_
    if (is.null(stat_val) || length(stat_val) == 0) stat_val <- NA_real_
    if (is.null(df1) || length(df1) == 0) df1 <- NA_integer_
    
    link_txt <- switch(fam_label,
      "Poisson" = "distribui\u00e7\u00e3o de Poisson com fun\u00e7\u00e3o de liga\u00e7\u00e3o logar\u00edtmica",
      "Quasipoisson" = "distribui\u00e7\u00e3o Quasipoisson (corre\u00e7\u00e3o para sobredispers\u00e3o) com fun\u00e7\u00e3o de liga\u00e7\u00e3o logar\u00edtmica",
      "Binomial Negativa" = "distribui\u00e7\u00e3o Binomial Negativa com fun\u00e7\u00e3o de liga\u00e7\u00e3o logar\u00edtmica",
      "distribui\u00e7\u00e3o de Poisson com fun\u00e7\u00e3o de liga\u00e7\u00e3o logar\u00edtmica"
    )
    
    p_txt <- if(is.na(pv)) "N/A" else if(pv < 0.001) "< 0.001" else sprintf("= %.3f", pv)
    sig_txt <- if(is.na(pv) || pv > 0.05) "n\u00e3o significativo" else "significativo"
    posthoc_txt <- if(!is.na(pv) && pv <= 0.05) " As taxas estimadas foram comparadas usando contrastes de Tukey (p < 0.05)." else ""
    disp_txt <- sprintf(" A raz\u00e3o de dispers\u00e3o (Pearson \u03c7\u00b2/gl) foi de %.2f, %s.", disp$ratio,
                        if(disp$ratio > 1.5) "indicando sobredispers\u00e3o" else "indicando dispers\u00e3o adequada")
    
    txt <- sprintf("Para a vari\u00e1vel de contagem, ajustou-se um Modelo Linear Generalizado (GLM) assumindo %s. A an\u00e1lise de %s revelou que o tratamento teve efeito %s (\u03c7\u00b2(%s) = %.2f, p %s).%s%s",
                   link_txt, test_name, sig_txt,
                   if(!is.na(df1)) as.character(df1) else "?",
                   if(!is.na(stat_val)) stat_val else 0,
                   p_txt, posthoc_txt, disp_txt)
    
    export$txt_glm <- txt
    tags$div(class="interp-box", h5(icon("quote-left")," Texto Sugerido para Publica\u00e7\u00e3o"), p(txt),
             tags$small(class="text-muted", paste0("Modelo: GLM ", fam_label, " | Dispers\u00e3o: ", round(disp$ratio,2))))
  })
  
  output$dl_plot_glm     <- downloadHandler("glm_plot.pdf", function(f) ggsave(f, export$plot_glm, device="pdf", width=cur_pdf_w(), height=cur_pdf_h()))
  output$dl_plot_glm_png <- downloadHandler("glm_plot.png", function(f) ggsave(f, export$plot_glm, device="png", width=cur_pdf_w(), height=cur_pdf_h(), dpi=300))
  output$dl_plot_glm_svg <- downloadHandler("glm_plot.svg", function(f) ggsave(f, export$plot_glm, device="svg", width=cur_pdf_w(), height=cur_pdf_h()))
  output$dl_res_glm  <- downloadHandler("glm_res.txt",  function(f) writeLines(export$res_glm, f))
  output$dl_report_glm <- downloadHandler("glm_report.txt", function(f) writeLines(export$txt_glm, f))
  
  observeEvent(input$btn_copy_glm, {
    req(export$res_glm)
    tryCatch({
      clipr::write_clip(export$res_glm)
      showNotification("Relatório completo copiado para a área de transferência!", type="message")
    }, error = function(e) { print(e$message);
      showNotification("Erro ao copiar. Verifique se o pacote 'clipr' está instalado e funcional.", type="error")
    })
  })

  
  # ══════════════════════════════════════════════════════════
  #  TAB CORRELAÇÃO
  # ══════════════════════════════════════════════════════════
  
  output$ui_cor_cols <- renderUI({
    req(rv$data)
    df <- rv$data
    num_cols <- names(df)[sapply(df, is.numeric)]
    selectizeInput("cor_cols", "Variáveis Contínuas:", choices=num_cols, selected=num_cols[1:min(5, length(num_cols))], multiple=TRUE)
  })
  
  observeEvent(input$cor_cols, { rv$cor_cols <- input$cor_cols })
  
  cor_data <- reactive({
    req(rv$data, rv$cor_cols)
    if(length(rv$cor_cols) < 2) return(NULL)
    df <- rv$data[, rv$cor_cols, drop=FALSE]
    tidyr::drop_na(df)
  })
  
  output$plot_cor <- renderPlot({
    req(cor_data())
    df <- cor_data()
    if(ncol(df) < 2) return(NULL)
    
    method <- input$cor_method %||% "pearson"
    export$cor_data <- df
    export$cor_method <- method
    cormat <- cor(df, method=method)
    
    if (requireNamespace("corrplot", quietly=TRUE)) {
      corrplot::corrplot(cormat, method="color", type="upper", addCoef.col="black", 
                         tl.col="black", tl.srt=45, diag=FALSE)
    } else {
      plot(df, main="Matriz de Dispersão")
    }
  })
  
  output$tbl_cor <- renderDT({
    req(cor_data())
    df <- cor_data()
    n <- ncol(df)
    if(n < 2) return(NULL)
    
    method <- input$cor_method %||% "pearson"
    
    res_list <- list()
    for (i in 1:(n-1)) {
      for (j in (i+1):n) {
        test <- suppressWarnings(cor.test(df[[i]], df[[j]], method=method))
        res_list[[length(res_list)+1]] <- data.frame(
          Var1 = names(df)[i],
          Var2 = names(df)[j],
          Correlacao = round(test$estimate, 3),
          P_valor = format.pval(test$p.value, eps=0.001)
        )
      }
    }
    res_df <- do.call(rbind, res_list)
    datatable(res_df, rownames=FALSE, options=list(dom="t", pageLength=15), selection="none")
  })

  # ══════════════════════════════════════════════════════════
  #  TAB 5 - REGRESSAO
  # ══════════════════════════════════════════════════════════
  reg_data <- reactive({
    req(rv$data, rv$reg_x, rv$reg_y)
    tt <- input$transf_reg %||% "Nenhuma"
    df <- rv$data
    if (tt != "Nenhuma") { df[["y_transf"]] <- apply_transform(df[[rv$reg_y]], tt) } else { df[["y_transf"]] <- df[[rv$reg_y]] }
    df
  })
  
  modelo_reg <- reactive({
    req(reg_data())
    df <- reg_data()
    if(input$reg_type=="Linear") lm(as.formula(paste("y_transf", "~", rv$reg_x)), data=df)
    else lm(as.formula(paste("y_transf ~ poly(", rv$reg_x, ", 2, raw=TRUE)")), data=df)
  })
  
  output$plot_reg <- renderPlot({
    req(reg_data(), modelo_reg())
    df <- reg_data(); m <- modelo_reg()
    pal <- cur_palette(); fsize <- cur_fsize(); alpha_val <- cur_alpha(); leg_pos <- cur_legend()
    theme_fn <- cur_theme(); tt <- input$transf_reg %||% "Nenhuma"
    ylabel <- transf_label(rv$reg_y, tt)
    
    p <- ggplot(df, aes(x=!!sym(rv$reg_x), y=y_transf))
    if (cur_jitter()) p <- p + geom_point(size=cur_pt_size(), color=pal[1], alpha=alpha_val)
    
    if(input$reg_type=="Linear"){
      p <- p + geom_smooth(method="lm", se=TRUE, color=pal[4], fill=pal[4], alpha=.15) +
        stat_cor(method="pearson", size=5, label.x.npc="left", label.y.npc="top")
      
      if (!is.null(input$reg_show_eq) && input$reg_show_eq) {
        p <- p + stat_regline_equation(label.x.npc="left", label.y.npc=0.90, size=5)
      }
      p <- p + labs(title="Regressão Linear Simples", y=ylabel)
    } else {
      p <- p + geom_smooth(method="lm", se=TRUE, formula=y~poly(x,2), color=pal[3], fill=pal[3], alpha=.12)
      
      if (!is.null(input$reg_show_eq) && input$reg_show_eq) {
        p <- p + stat_regline_equation(formula = y ~ poly(x, 2, raw = TRUE), label.x.npc="left", label.y.npc="top", size=5)
      }

      if(length(coef(m))==3 && !is.na(coef(m)[3])){
        b <- coef(m)[2]; a <- coef(m)[3]; dm <- -b/(2*a)
        ym <- predict(m, newdata=setNames(data.frame(dm), rv$reg_x))
        p <- p + geom_vline(xintercept=dm, linetype="dashed", color=pal[3], linewidth=.8) +
          geom_point(aes(x=dm, y=ym), color=pal[3], size=4, shape=18)
        
        if (is.null(input$reg_show_opt) || input$reg_show_opt) {
          p <- p + annotate("label", x=dm, y=ym, label=paste0("Ponto ótimo\nx=",round(dm,1)," | y=",round(ym,2)),
                   fill="#FDF2F2", color=pal[3], fontface="bold", size=3.8, label.padding=unit(.4,"lines"))
        }
      }
      p <- p + labs(title="Regressão Polinomial (Quadrática)", y=ylabel)
    }
    
    p <- p + theme_fn(base_size=fsize) +
      theme(plot.title=element_text(face="bold"), legend.position=leg_pos)
    p <- apply_custom_labels(p, input)
    export$plot_reg <- p; p
  })
  
  output$res_reg <- renderPrint({
    req(modelo_reg())
    tt <- input$transf_reg %||% "Nenhuma"
    out <- capture.output({
      if(tt!="Nenhuma") cat(paste0("*** Transformação aplicada: ", tt, " ***\n\n"))
      print(summary(modelo_reg()))
    })
    export$res_reg <- out; cat(out, sep="\n")
    export$model_reg <- modelo_reg()
  })
  
  output$interp_reg <- renderUI({
    req(modelo_reg()); m <- modelo_reg(); s <- summary(m)
    tt <- input$transf_reg %||% "Nenhuma"
    r2 <- round(s$r.squared,4); r2a <- round(s$adj.r.squared,4)
    pf <- pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3], lower.tail=FALSE)
    
    items <- list()
    if (tt != "Nenhuma") items <- c(items, list(tags$div(class="interp-item", HTML(paste0("<b>Transformação:</b> ", tt, " aplicada à variável resposta.")))))
    items <- c(items, list(
      tags$div(class="interp-item", HTML(paste0("<b>Modelo:</b> ", input$reg_type))),
      tags$div(class="interp-item", HTML(paste0("<b>R² Ajustado:</b> ", r2a, " — O modelo explica <b>", round(r2a*100,1), "%</b> da variação de ", transf_label(rv$reg_y, tt), "."))),
      tags$div(class="interp-item", HTML(paste0("<b>Significância global:</b> ", badge_sig(pf))))
    ))
    if(input$reg_type!="Linear" && length(coef(m))==3 && !is.na(coef(m)[3])){
      b <- coef(m)[2]; a <- coef(m)[3]; dm <- -b/(2*a)
      ym <- predict(m, newdata=setNames(data.frame(dm), rv$reg_x))
      tipo <- if(a<0) "máximo" else "mínimo"
      items <- c(items, list(tags$div(class="interp-item", HTML(paste0("<b>Ponto ",tipo,":</b> A dose ótima estimada é <b>x = ",round(dm,1),"</b>, resultando em <b>y = ",round(ym,2),"</b> (escala ",if(tt!="Nenhuma") "transformada" else "original",").")))))
    }
    export$interp_reg_html <- as.character(tagList(items))
    tags$div(class="interp-box", h5(icon("lightbulb")," Interpretação da Regressão"), tagList(items))
  })
  
  output$sci_reg <- renderUI({
    req(modelo_reg())
    m <- modelo_reg()
    s <- summary(m)
    r2 <- s$r.squared
    pf_val <- pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3], lower.tail=FALSE)
    
    if (input$reg_type == "Linear") {
      txt <- sprintf("A relação entre as variáveis foi avaliada através de regressão linear simples. O modelo ajustado foi %s (F(%d, %d) = %.2f, p %s), sendo capaz de explicar %.1f%% da variação dos dados (R² = %.3f). A equação ajustada foi y = %.3f %s %.3fx.",
                     if(pf_val <= 0.05) "significativo" else "não significativo",
                     s$fstatistic[2], s$fstatistic[3], s$fstatistic[1],
                     if(pf_val < 0.001) "< 0.001" else sprintf("= %.3f", pf_val),
                     r2*100, r2, coef(m)[1], if(coef(m)[2] >= 0) "+" else "-", abs(coef(m)[2]))
    } else {
      c1 <- coef(m)[1]; c2 <- coef(m)[2]; c3 <- if(length(coef(m))==3) coef(m)[3] else 0
      txt <- sprintf("A relação entre as variáveis foi descrita através de um modelo de regressão polinomial quadrática. O modelo ajustado foi %s (F(%d, %d) = %.2f, p %s), explicando %.1f%% da variação dos dados (R² = %.3f). A equação ajustada foi y = %.3f %s %.3fx %s %.3fx².",
                     if(pf_val <= 0.05) "significativo" else "não significativo",
                     s$fstatistic[2], s$fstatistic[3], s$fstatistic[1],
                     if(pf_val < 0.001) "< 0.001" else sprintf("= %.3f", pf_val),
                     r2*100, r2, c1, if(c2 >= 0) "+" else "-", abs(c2), if(c3 >= 0) "+" else "-", abs(c3))
    }
    
    export$txt_reg <- txt
    tags$div(class="interp-box", h5(icon("quote-left")," Texto Sugerido para Publicação"), p(txt),
             tags$small(class="text-muted", "Nota: Adapte o texto incluindo o nome das variáveis reais nos coeficientes da equação."))
  })
  
  # ── Diagnóstico de Resíduos da Regressão (4 gráficos clássicos) ────────────
  output$plot_reg_diag <- renderPlot({
    req(modelo_reg())
    m <- modelo_reg()
    par(mfrow = c(2, 2), mar = c(4, 4, 3, 1), oma = c(0, 0, 2, 0))
    plot(m, which = 1, main = "Resíduos vs Ajustados",
         sub.caption = "", caption = "")
    plot(m, which = 2, main = "QQ-Plot dos Resíduos",
         sub.caption = "", caption = "")
    plot(m, which = 3, main = "Scale-Location",
         sub.caption = "", caption = "")
    plot(m, which = 4, main = "Distância de Cook (Pontos Influentes)",
         sub.caption = "", caption = "")
    mtext(paste0("Diagnóstico do Modelo: ", input$reg_type,
                 "  |  R² = ", round(summary(m)$r.squared, 3)),
          outer = TRUE, cex = 1.05, font = 2, col = "#1A73E8")
    par(mfrow = c(1, 1))
  })

  output$dl_plot_reg     <- downloadHandler("regressao_plot.pdf", function(f) ggsave(f, export$plot_reg, device="pdf", width=cur_pdf_w(), height=cur_pdf_h()))
  output$dl_plot_reg_png <- downloadHandler("regressao_plot.png", function(f) ggsave(f, export$plot_reg, device="png", width=cur_pdf_w(), height=cur_pdf_h(), dpi=300))
  output$dl_plot_reg_svg <- downloadHandler("regressao_plot.svg", function(f) ggsave(f, export$plot_reg, device="svg", width=cur_pdf_w(), height=cur_pdf_h()))
  output$dl_res_reg  <- downloadHandler("regressao_res.txt",  function(f) writeLines(export$res_reg, f))
  
  observeEvent(input$btn_copy_reg, {
    req(export$res_reg)
    tryCatch({
      clipr::write_clip(export$res_reg)
      showNotification("Relatório completo copiado para a área de transferência!", type="message")
    }, error = function(e) { print(e$message);
      showNotification("Erro ao copiar. Verifique se o pacote 'clipr' está instalado e funcional.", type="error")
    })
  })
  
  # ══════════════════════════════════════════════════════════
  #  TAB 6 - AUDPC
  # ══════════════════════════════════════════════════════════
  audpc_summary <- reactive({
    req(rv$data, rv$audpc_time, rv$audpc_sev, rv$audpc_trat)
    rv$data |> group_by(!!sym(rv$audpc_trat), !!sym(rv$audpc_time)) |>
      summarise(
        mean_sev = mean(!!sym(rv$audpc_sev)*100, na.rm=TRUE),
        sd_sev   = sd(!!sym(rv$audpc_sev)*100, na.rm=TRUE),
        n_rep    = sum(!is.na(!!sym(rv$audpc_sev))),
        se_sev   = sd_sev / sqrt(pmax(n_rep, 1)),  # Erro Padrão (SE)
        .groups  = "drop"
      )
  })
  
  output$plot_audpc_curve <- renderPlot({
    req(audpc_summary())
    df_s <- audpc_summary()
    df_s <- apply_plot_levels(df_s, rv$audpc_trat, input$custom_levels_1)
    pal <- cur_palette(); fsize <- cur_fsize(); alpha_val <- cur_alpha(); leg_pos <- cur_legend()
    theme_fn <- cur_theme()
    
    p <- ggplot(df_s, aes(x=!!sym(rv$audpc_time), y=mean_sev, group=!!sym(rv$audpc_trat), color=!!sym(rv$audpc_trat), fill=!!sym(rv$audpc_trat))) +
      geom_area(alpha=alpha_val*0.4, position="identity", linewidth=0) +
      geom_line(linewidth=1.1) + geom_point(size=cur_pt_size()) +
      geom_errorbar(aes(ymin=mean_sev-se_sev, ymax=mean_sev+se_sev), width=.6, linewidth=.5) +  # SE, não SD
      scale_color_manual(values=pal) + scale_fill_manual(values=pal) +
      theme_fn(base_size=fsize) +
      theme(plot.title=element_text(face="bold"), plot.subtitle=element_text(color="gray40"), legend.position=leg_pos) +
      labs(x="Dias", y="Severidade Média (%)", color="Tratamento", fill="Tratamento",
           title="Curva de Progresso da Doença",
           subtitle="Barras de erro: Média ± Erro Padrão (SE) | Área sombreada representa a AUDPC")
    p <- apply_custom_labels(p, input)
    export$plot_audpc_curve <- p; p
  })
  
  # ─── Epidemiological Model Plot ──────────────────────────
  output$plot_epi <- renderPlot({
    req(rv$data, rv$audpc_time, rv$audpc_sev, rv$audpc_trat)
    pal <- cur_palette(); fsize <- cur_fsize(); theme_fn <- cur_theme(); leg_pos <- cur_legend()
    
    df <- rv$data
    trats <- unique(df[[rv$audpc_trat]])
    epi_model_name <- input$epi_model %||% "Logístico"
    
    all_fits <- lapply(trats, function(tr) {
      sub <- df[df[[rv$audpc_trat]] == tr, ]
      time_v <- sub[[rv$audpc_time]]
      sev_v <- sub[[rv$audpc_sev]]
      mean_df <- aggregate(sev_v, by=list(time=time_v), FUN=mean, na.rm=TRUE)
      names(mean_df) <- c("time", "sev")
      mean_df$trat <- tr
      mean_df
    })
    plot_df <- do.call(rbind, all_fits)
    
    p <- ggplot(plot_df, aes(x=time, y=sev, color=trat)) +
      geom_point(size=cur_pt_size()) +
      geom_line(linewidth=0.8) +
      scale_color_manual(values=pal) +
      theme_fn(base_size=fsize) +
      theme(plot.title=element_text(face="bold"), legend.position=leg_pos) +
      labs(x="Tempo", y="Severidade", color="Tratamento",
           title=paste0("Modelo Epidemiológico — ", epi_model_name))
    p <- apply_custom_labels(p, input)
    export$plot_epi <- p; p
  })
  
  output$res_epi <- renderPrint({
    req(rv$data, rv$audpc_time, rv$audpc_sev, rv$audpc_trat)
    df <- rv$data
    trats <- unique(df[[rv$audpc_trat]])
    epi_model_name <- input$epi_model %||% "Logístico"
    
    for (tr in trats) {
      sub <- df[df[[rv$audpc_trat]] == tr, ]
      time_v <- sub[[rv$audpc_time]]
      sev_v <- sub[[rv$audpc_sev]]
      mean_df <- aggregate(sev_v, by=list(time=time_v), FUN=mean, na.rm=TRUE)
      names(mean_df) <- c("time", "y")
      
      cat(paste0("=== Tratamento: ", tr, " (", epi_model_name, ") ===\n"))
      tryCatch({
        # Mapear nome do modelo para o formato aceito pelo epifitter::fit_lin()
        epi_key <- switch(epi_model_name,
          "Monomolecular" = "Monomolecular",
          "Logístico"     = "Logistic",
          "Gompertz"      = "Gompertz",
          "Logistic"  # fallback
        )
        fit <- fit_lin(time=mean_df$time, y=mean_df$y, model=epi_key)
        cat(paste0("  r  = ", round(fit$Stats[2], 5), "\n"))
        cat(paste0("  y0 = ", round(fit$Stats[1], 5), "\n"))
        cat(paste0("  R² = ", round(fit$Stats[3], 4), "\n\n"))
      }, error=function(e) cat(paste0("  Erro no ajuste: ", e$message, "\n\n")))
    }
  })
  
  audpc_computed <- reactive({
    req(rv$data, rv$audpc_time, rv$audpc_sev, rv$audpc_trat, rv$audpc_rep)
    dados_audpc <- rv$data |> group_by(!!sym(rv$audpc_trat), !!sym(rv$audpc_rep)) |>
      summarise(audpc_val=AUDPC(time=!!sym(rv$audpc_time), y=!!sym(rv$audpc_sev)), .groups="drop")
    
    if (input$use_raudpc) {
      max_t <- max(rv$data[[rv$audpc_time]], na.rm=TRUE)
      min_t <- min(rv$data[[rv$audpc_time]], na.rm=TRUE)
      if ((max_t - min_t) > 0) { dados_audpc$audpc_val <- dados_audpc$audpc_val / (max_t - min_t) }
    }
    
    tt <- input$transf_audpc %||% "Nenhuma"
    if (tt != "Nenhuma") {
      dados_audpc$audpc_val <- apply_transform(dados_audpc$audpc_val, tt)
    }
    dados_audpc
  })
  
  output$plot_audpc_bar <- renderPlot({
    req(audpc_computed())
    dados_audpc <- audpc_computed()
    pal <- cur_palette(); fsize <- cur_fsize(); alpha_val <- cur_alpha()
    theme_fn <- cur_theme(); tt <- input$transf_audpc %||% "Nenhuma"
    ylabel <- if(tt!="Nenhuma") paste0("Média da ", transf_label("AUDPC", tt), " (IC 95%)") else "Média da AUDPC (IC 95%)"
    
    m <- lm(as.formula(paste("audpc_val ~", rv$audpc_trat)), data=dados_audpc)
    letras_df <- clean_cld(emmeans(m, as.formula(paste("~", rv$audpc_trat))))
    letras_df <- apply_plot_levels(letras_df, rv$audpc_trat, input$custom_levels_1)
    
    p <- ggplot(letras_df, aes(x=!!sym(rv$audpc_trat), y=emmean, fill=!!sym(rv$audpc_trat))) +
      geom_col(color="black", width=.55, alpha=alpha_val) +
      geom_point(size=cur_pt_size()+0.5) +
      geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), width=.15) +
      geom_text(aes(y=upper.CL, label=.group), vjust=-.6, fontface="bold", size=5) +
      scale_fill_manual(values=pal) + scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
      theme_fn(base_size=fsize) +
      theme(legend.position="none", plot.title=element_text(face="bold")) +
      labs(y=ylabel, title="AUDPC — Comparação (Tukey)")
    p <- apply_custom_labels(p, input)
    export$plot_audpc_bar <- p; p
  })
  
  output$res_audpc <- renderPrint({
    req(audpc_computed())
    dados_audpc <- audpc_computed()
    tt <- input$transf_audpc %||% "Nenhuma"
    m <- lm(as.formula(paste("audpc_val ~", rv$audpc_trat)), data=dados_audpc)
    out <- capture.output({
      if(tt!="Nenhuma") cat(paste0("*** Transformação aplicada: ", tt, " nos valores de AUDPC ***\n\n"))
      cat("=== ANOVA da AUDPC ===\n"); print(anova(m))
    })
    export$res_audpc <- out; cat(out, sep="\n")
  })
  
  output$interp_audpc <- renderUI({
    req(audpc_computed())
    dados_audpc <- audpc_computed()
    tt <- input$transf_audpc %||% "Nenhuma"
    m <- lm(as.formula(paste("audpc_val ~", rv$audpc_trat)), data=dados_audpc); av <- anova(m)
    pv <- av[rv$audpc_trat, "Pr(>F)"]
    medias <- dados_audpc |> group_by(!!sym(rv$audpc_trat)) |> summarise(m=mean(audpc_val, na.rm=TRUE), .groups="drop")
    menor <- medias[[rv$audpc_trat]][which.min(medias$m)]; maior <- medias[[rv$audpc_trat]][which.max(medias$m)]
    vm <- round(min(medias$m),3); vM <- round(max(medias$m),3)
    concl <- if(!is.na(pv) && pv<=0.05) "A AUDPC difere significativamente entre tratamentos, indicando que <b>os tratamentos influenciam o progresso da doença ao longo do tempo</b>." else "Não há diferença significativa da AUDPC entre os tratamentos, sugerindo que <b>o progresso da doença foi semelhante</b> entre os grupos avaliados."
    
    items <- list()
    if (tt != "Nenhuma") items <- c(items, list(tags$div(class="interp-item", HTML(paste0("<b>Transformação:</b> ", tt, " aplicada aos valores de AUDPC.")))))
    items <- c(items, list(
      tags$div(class="interp-item", HTML(paste0("<b>O que é AUDPC?</b> É a Área Abaixo da Curva de Progresso da Doença — uma medida que resume a epidemia ao longo do tempo. Quanto maior a AUDPC, maior a severidade acumulada."))),
      tags$div(class="interp-item", HTML(paste0("<b>Menor AUDPC:</b> ", menor, " (AUDPC média = ", vm, ")"))),
      tags$div(class="interp-item", HTML(paste0("<b>Maior AUDPC:</b> ", maior, " (AUDPC média = ", vM, ")"))),
      tags$div(class="interp-item", HTML(paste0("<b>Significância (ANOVA):</b> ", badge_sig(pv)))),
      tags$div(class="interp-item", HTML(paste0("<b>Conclusão:</b> ", concl)))
    ))
    export$interp_audpc_html <- as.character(tagList(items))
    tags$div(class="interp-box", h5(icon("lightbulb")," Interpretação da AUDPC"), tagList(items))
  })
  
  output$sci_audpc <- renderUI({
    req(audpc_computed())
    dados_audpc <- audpc_computed()
    m <- lm(as.formula(paste("audpc_val ~", rv$audpc_trat)), data=dados_audpc)
    av <- anova(m)
    pv <- av[rv$audpc_trat, "Pr(>F)"]
    fval <- av[rv$audpc_trat, "F value"]
    df1 <- av[rv$audpc_trat, "Df"]; df2 <- av["Residuals", "Df"]
    
    txt <- sprintf("O progresso da doença foi resumido pelo cálculo da Área Abaixo da Curva de Progresso da Doença (AUDPC) utilizando o método de integração trapezoidal. A análise de variância indicou que %s diferença significativa na epidemia entre os tratamentos (F(%d, %d) = %.2f, p %s). %s",
                   if(!is.na(pv) && pv <= 0.05) "houve" else "não houve",
                   df1, df2, fval,
                   if(!is.na(pv) && pv < 0.001) "< 0.001" else sprintf("= %.3f", pv),
                   if(!is.na(pv) && pv <= 0.05) "As médias da AUDPC foram separadas pelo teste de Tukey (p < 0.05)." else "")
                   
    export$txt_audpc <- txt
    tags$div(class="interp-box", h5(icon("quote-left")," Texto Sugerido para Publicação"), p(txt),
             tags$small(class="text-muted", "Nota: Revise o texto antes de publicá-lo para adequar ao seu estudo."))
  })
  
  output$dl_plot_audpc_curve     <- downloadHandler("audpc_curva.pdf",  function(f) ggsave(f, export$plot_audpc_curve, device="pdf", width=cur_pdf_w(), height=cur_pdf_h()))
  output$dl_plot_audpc_curve_png <- downloadHandler("audpc_curva.png",  function(f) ggsave(f, export$plot_audpc_curve, device="png", width=cur_pdf_w(), height=cur_pdf_h(), dpi=300))
  output$dl_plot_audpc_curve_svg <- downloadHandler("audpc_curva.svg",  function(f) ggsave(f, export$plot_audpc_curve, device="svg", width=cur_pdf_w(), height=cur_pdf_h()))
  output$dl_plot_audpc_bar       <- downloadHandler("audpc_barras.pdf", function(f) ggsave(f, export$plot_audpc_bar,   device="pdf", width=cur_pdf_w(), height=cur_pdf_h()))
  output$dl_plot_audpc_bar_png   <- downloadHandler("audpc_barras.png", function(f) ggsave(f, export$plot_audpc_bar,   device="png", width=cur_pdf_w(), height=cur_pdf_h(), dpi=300))
  output$dl_plot_audpc_bar_svg   <- downloadHandler("audpc_barras.svg", function(f) ggsave(f, export$plot_audpc_bar,   device="svg", width=cur_pdf_w(), height=cur_pdf_h()))
  output$dl_plot_epi             <- downloadHandler("epi_plot.pdf",     function(f) ggsave(f, export$plot_epi,          device="pdf", width=cur_pdf_w(), height=cur_pdf_h()))
  output$dl_plot_epi_png         <- downloadHandler("epi_plot.png",     function(f) ggsave(f, export$plot_epi,          device="png", width=cur_pdf_w(), height=cur_pdf_h(), dpi=300))
  output$dl_plot_epi_svg         <- downloadHandler("epi_plot.svg",     function(f) ggsave(f, export$plot_epi,          device="svg", width=cur_pdf_w(), height=cur_pdf_h()))
  output$dl_res_audpc            <- downloadHandler("audpc_res.txt",    function(f) writeLines(export$res_audpc, f))
  
  observeEvent(input$btn_copy_audpc, {
    req(export$res_audpc)
    tryCatch({
      clipr::write_clip(export$res_audpc)
      showNotification("Relatório completo copiado para a área de transferência!", type="message")
    }, error = function(e) { print(e$message);
      showNotification("Erro ao copiar. Verifique se o pacote 'clipr' está instalado e funcional.", type="error")
    })
  })
  
  # ══════════════════════════════════════════════════════════
  #  TAB 7 - MAPAS
  # ══════════════════════════════════════════════════════════
  world_map_static <- tryCatch(st_read("data/world_map.geojson", quiet=TRUE), error=function(e) NULL)
  br_states_static <- tryCatch(st_read("data/br_states.geojson", quiet=TRUE), error=function(e) NULL)
  
  output$ui_mapa_lon <- renderUI({ selectInput("mapa_lon", "Longitude (X):", num_cols(), selected = rv$mapa_lon) })
  output$ui_mapa_lat <- renderUI({ selectInput("mapa_lat", "Latitude (Y):", num_cols(), selected = rv$mapa_lat) })
  output$ui_mapa_var <- renderUI({ selectInput("mapa_var", "Variável:", c("Nenhum", num_cols()), selected = rv$mapa_var) })
  
  output$ui_mapa_modo <- renderUI({
    req(rv$mapa_var != "Nenhum")
    radioButtons("mapa_modo", "Mapear em:", choices=c("Ambos (Cor e Tamanho)", "Apenas Cor", "Apenas Tamanho"), selected=rv$mapa_modo)
  })
  
  output$ui_mapa_regiao <- renderUI({
    country_choices <- if(!is.null(world_map_static)) sort(world_map_static$name) else c("Brazil", "Ethiopia")
    br_states <- c("Acre", "Alagoas", "Amapá", "Amazonas", "Bahia", "Ceará", "Distrito Federal", "Espírito Santo", "Goiás", "Maranhão", "Mato Grosso", "Mato Grosso do Sul", "Minas Gerais", "Pará", "Paraíba", "Paraná", "Pernambuco", "Piauí", "Rio de Janeiro", "Rio Grande do Norte", "Rio Grande do Sul", "Rondônia", "Roraima", "Santa Catarina", "São Paulo", "Sergipe", "Tocantins")
    br_state_choices <- paste0(br_states, " (Estado)")
    choices <- c("Automático (Zoom nos dados)", "Mundo", "América do Sul", "Brasil (Estados)", br_state_choices, country_choices)
    sel <- rv$mapa_regiao_selected %||% "Mundo"
    selectInput("mapa_regiao", "Fronteira:", choices=unique(choices), selected=sel)
  })
  
  observeEvent(input$mapa_regiao, { rv$mapa_regiao_selected <- input$mapa_regiao })
  observeEvent(input$mapa_lon, { rv$mapa_lon <- input$mapa_lon })
  observeEvent(input$mapa_lat, { rv$mapa_lat <- input$mapa_lat })
  observeEvent(input$mapa_var, { rv$mapa_var <- input$mapa_var })
  observeEvent(input$mapa_modo, { rv$mapa_modo <- input$mapa_modo })

  output$mapa_info_paises <- renderUI({
    req(rv$data, rv$mapa_lon, rv$mapa_lat, world_map_static)
    df <- rv$data
    if (!is.numeric(df[[rv$mapa_lon]]) || !is.numeric(df[[rv$mapa_lat]])) return(NULL)
    
    tryCatch({
      df_clean <- df[!is.na(df[[rv$mapa_lon]]) & !is.na(df[[rv$mapa_lat]]), ]
      if(nrow(df_clean) == 0) return(NULL)
      
      pts <- st_as_sf(df_clean, coords = c(rv$mapa_lon, rv$mapa_lat), crs = 4326)
      inter <- st_join(pts, world_map_static, join = st_intersects)
      
      paises <- unique(inter$name)
      paises <- paises[!is.na(paises)]
      if(length(paises) > 0) {
        msg <- paste(paises, collapse=", ")
        if("Brazil" %in% paises) {
          br <- br_states_static
          if(!is.null(br)) {
            inter_br <- st_join(pts, br, join = st_intersects)
            estados <- unique(inter_br$name)
            estados <- estados[!is.na(estados)]
            if(length(estados) > 0) {
              msg <- paste0(msg, " (Estado: ", paste(estados, collapse=", "), ")")
            }
          }
        }
        tags$div(class="alert alert-info", style="padding:10px; margin-bottom:15px;",
          icon("location-dot"), HTML(paste0(" <b>Detecção Espacial:</b> Os pontos carregados estão localizados em: <b>", msg, "</b>"))
        )
      } else {
        tags$div(class="alert alert-warning", style="padding:10px; margin-bottom:15px;",
          icon("water"), HTML(" <b>Detecção Espacial:</b> Os pontos não coincidiram com nenhum país conhecido (podem estar no oceano ou as coordenadas invertidas).")
        )
      }
    }, error=function(e) NULL)
  })

  output$plot_mapa <- renderPlot({
    req(rv$data, rv$mapa_lon, rv$mapa_lat)
    df <- rv$data
    req(is.numeric(df[[rv$mapa_lon]]), is.numeric(df[[rv$mapa_lat]]))
    pal <- cur_palette(); fsize <- cur_fsize(); theme_fn <- cur_theme()
    
    reg <- input$mapa_regiao
    if(reg == "Automático (Zoom nos dados)" || reg == "Mundo" || is.null(reg)){
      base_map <- world_map_static
      if (!is.null(reg) && reg == "Automático (Zoom nos dados)") {
        tryCatch({
          df_clean <- df[!is.na(df[[rv$mapa_lon]]) & !is.na(df[[rv$mapa_lat]]), ]
          if(nrow(df_clean) > 0) {
            pts <- st_as_sf(df_clean, coords = c(rv$mapa_lon, rv$mapa_lat), crs = 4326)
            inter <- st_join(pts, world_map_static, join = st_intersects)
            paises <- unique(inter$name)
            if(length(paises) == 1 && paises[1] == "Brazil") {
              base_map <- br_states_static
            }
          }
        }, error=function(e) NULL)
      }
    } else if(reg == "América do Sul"){
      base_map <- if(!is.null(world_map_static)) subset(world_map_static, continent == "South America") else NULL
    } else if(reg == "Brasil (Estados)"){
      base_map <- br_states_static
    } else if(grepl(" \\(Estado\\)$", reg)){
      state_name <- sub(" \\(Estado\\)$", "", reg)
      base_map <- if(!is.null(br_states_static)) subset(br_states_static, name == state_name | name_en == state_name) else NULL
    } else {
      # Um país específico selecionado da lista
      base_map <- if(!is.null(world_map_static)) subset(world_map_static, name == reg) else NULL
    }
    
    p <- ggplot()
    if(!is.null(base_map)) {
      p <- p + geom_sf(data = base_map, fill = "antiquewhite", color = "darkgrey")
    }
      
    if (rv$mapa_var != "Nenhum" && rv$mapa_var %in% names(df)) {
      modo <- rv$mapa_modo %||% "Ambos (Cor e Tamanho)"
      if(modo == "Apenas Cor"){
        p <- p + geom_point(data = df, aes(x = !!sym(rv$mapa_lon), y = !!sym(rv$mapa_lat), color = !!sym(rv$mapa_var)), size = 3, alpha = 0.8) +
          scale_color_gradient(low = "lightgrey", high = pal[1]) + labs(color = rv$mapa_var)
      } else if(modo == "Apenas Tamanho"){
        p <- p + geom_point(data = df, aes(x = !!sym(rv$mapa_lon), y = !!sym(rv$mapa_lat), size = !!sym(rv$mapa_var)), color = pal[3], alpha = 0.8) +
          labs(size = rv$mapa_var)
      } else {
        p <- p + geom_point(data = df, aes(x = !!sym(rv$mapa_lon), y = !!sym(rv$mapa_lat), color = !!sym(rv$mapa_var), size = !!sym(rv$mapa_var)), alpha = 0.8) +
          scale_color_gradient(low = "lightgrey", high = pal[1]) + labs(color = rv$mapa_var, size = rv$mapa_var)
      }
    } else {
      p <- p + geom_point(data = df, aes(x = !!sym(rv$mapa_lon), y = !!sym(rv$mapa_lat)), color = pal[3], size = 3, alpha = 0.8)
    }
    
    p <- p + 
      theme_minimal(base_size = fsize) +
      theme(plot.title=element_text(face="bold")) +
      labs(title = paste("Mapa:", reg), x = "Longitude", y = "Latitude")
      
    if(reg == "Automático (Zoom nos dados)" && !is.null(base_map)){
      min_lon <- min(df[[rv$mapa_lon]], na.rm=TRUE); max_lon <- max(df[[rv$mapa_lon]], na.rm=TRUE)
      min_lat <- min(df[[rv$mapa_lat]], na.rm=TRUE); max_lat <- max(df[[rv$mapa_lat]], na.rm=TRUE)
      margin_lon <- abs(max_lon - min_lon) * 0.1
      margin_lat <- abs(max_lat - min_lat) * 0.1
      
      # Caso os dados não tenham variação em lat/lon (ex: único ponto)
      if(margin_lon == 0) margin_lon <- 1
      if(margin_lat == 0) margin_lat <- 1
      
      p <- p + coord_sf(xlim = c(min_lon - margin_lon, max_lon + margin_lon), 
                        ylim = c(min_lat - margin_lat, max_lat + margin_lat))
    } else if(!is.null(base_map)) {
      p <- p + coord_sf()
    }
    
    p <- apply_custom_labels(p, input)
    export$plot_mapa <- p; p
  })
  
  output$dl_plot_mapa     <- downloadHandler("Mapa.pdf", function(f) ggsave(f, export$plot_mapa, device="pdf", width=cur_pdf_w(), height=cur_pdf_h()))
  output$dl_plot_mapa_png <- downloadHandler("Mapa.png", function(f) ggsave(f, export$plot_mapa, device="png", width=cur_pdf_w(), height=cur_pdf_h(), dpi=300))
  output$dl_plot_mapa_svg <- downloadHandler("Mapa.svg", function(f) ggsave(f, export$plot_mapa, device="svg", width=cur_pdf_w(), height=cur_pdf_h()))

  # Forçar o Shiny a rodar esses outputs mesmo que o usuário não clique na aba (Lazy Evaluation Fix)
  outputOptions(output, "tbl_fatorial_anova", suspendWhenHidden = FALSE)
  outputOptions(output, "plot_anova_dharma", suspendWhenHidden = FALSE)
  outputOptions(output, "interp_anova", suspendWhenHidden = FALSE)
  outputOptions(output, "plot_cor", suspendWhenHidden = FALSE)
  outputOptions(output, "interp_ttest", suspendWhenHidden = FALSE)
  outputOptions(output, "interp_glm", suspendWhenHidden = FALSE)
  outputOptions(output, "interp_reg", suspendWhenHidden = FALSE)
  outputOptions(output, "interp_audpc", suspendWhenHidden = FALSE)
  outputOptions(output, "plot_glm_dharma", suspendWhenHidden = FALSE)
}

shinyApp(ui, server)
