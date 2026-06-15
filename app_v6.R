library(shiny)
library(bslib)
library(tidyverse)
library(ggplot2)
library(plotly)
library(readxl)
library(DT)
library(rstatix)
library(emmeans)
library(multcompView)
library(multcomp)
library(epifitter)
library(ggpubr)
library(DHARMa)
library(shinycssloaders)

# Configura a pasta de aulas para servir arquivos HTML estáticos
addResourcePath("aulas_html", "aulas")

# ─── Helpers ──────────────────────────────────────────────
clean_cld <- function(em_model) {
  letras <- cld(em_model, Letters = letters)
  letras$.group <- trimws(letras$.group)
  as.data.frame(letras)
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

# ─── Helper: Labels customizados ─────────────────────────
apply_custom_labels <- function(p, input) {
  if (!is.null(input$custom_title) && nzchar(trimws(input$custom_title))) p <- p + labs(title = input$custom_title)
  if (!is.null(input$custom_xlab)  && nzchar(trimws(input$custom_xlab)))  p <- p + labs(x = input$custom_xlab)
  if (!is.null(input$custom_ylab)  && nzchar(trimws(input$custom_ylab)))  p <- p + labs(y = input$custom_ylab)
  
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
  /* RendR Style Aesthetics */
  .bslib-page-navbar .navbar { border-bottom: 1px solid #E0E0E0 !important; background-color: #FFFFFF !important; padding-top: 0.5rem !important; padding-bottom: 0.5rem !important; }
  .navbar-brand { color: #202124 !important; font-weight: 700; font-size: 1.3rem; }
  .navbar-nav .nav-link { color: #5F6368 !important; font-weight: 500; }
  .navbar-nav .nav-link.active, .navbar-nav .nav-link:hover { color: #1A73E8 !important; }
  .top-banner { background-color: #1A73E8; color: #FFFFFF; text-align: center; padding: 6px 15px; font-size: 0.85rem; font-weight: 500; margin-top: -16px; margin-left: calc(-50vw + 50%); width: 100vw; margin-bottom: 0; }
  body { background-color: #FFFFFF; }
  
  /* RendR Container */
  .rendr-container { max-width: 800px; margin: 0 auto; padding: 0.5rem 2rem 1.5rem 2rem; text-align: center; }
  .rendr-title { font-size: 2.3rem; font-weight: 700; color: #202124; margin-bottom: 0.8rem; }
  .rendr-subtitle { font-size: 1.05rem; color: #5F6368; margin-bottom: 1.5rem; line-height: 1.5; }

  /* Dashed Box */
  .dashed-box { border: 2px dashed #DADCE0; border-radius: 12px; padding: 2.5rem 2rem; background: #FFFFFF; transition: all 0.2s ease; cursor: pointer; position: relative; }
  .dashed-box:hover { border-color: #1A73E8; background: #F8F9FA; }
  .dashed-box-title { font-size: 1.25rem; font-weight: 600; color: #202124; margin-bottom: 0.4rem; }
  .dashed-box-sub { font-size: 0.95rem; color: #5F6368; margin-bottom: 1.2rem; }
  .file-badges { display: flex; justify-content: center; gap: 10px; margin-bottom: 1rem; }
  .file-badge { background: #F1F3F4; border: 1px solid #E8EAED; border-radius: 16px; padding: 4px 12px; font-size: 0.75rem; font-weight: 600; color: #5F6368; }
  .file-limit { font-size: 0.8rem; color: #9AA0A6; }
  
  /* File Input Override inside dashed-box */
  .dashed-box .shiny-input-container { position: absolute !important; top: 0; left: 0; width: 100%; height: 100%; opacity: 0; cursor: pointer; margin: 0 !important; z-index: 100; overflow: hidden; }
  .dashed-box .input-group, .dashed-box .input-group-btn, .dashed-box .btn-file { height: 100%; width: 100%; cursor: pointer; margin: 0 !important; padding: 0 !important; }
  .dashed-box .btn-file { opacity: 0; position: absolute; top: 0; left: 0; }
  .dashed-box input[type='text'] { display: none; }
  
  /* Sheet selector center */
  #ui_sheet_selector { text-align: center; margin-top: 15px; }
  #ui_sheet_selector .control-label { text-align: center !important; display: block; font-weight: 600; margin-bottom: 8px; }
  #ui_sheet_selector .shiny-input-container { margin: 0 auto !important; }


  /* RendR Accordion */
  .rendr-accordion { margin-top: 2.5rem; text-align: left; }
  .rendr-accordion .accordion-item { border: 1px solid #E0E0E0; border-radius: 8px; margin-bottom: 0.5rem; overflow: hidden; }
  .rendr-accordion .accordion-button { background-color: #FFFFFF; color: #5F6368; font-weight: 500; font-size: 0.95rem; padding: 1rem 1.5rem; box-shadow: none !important; }
  .rendr-accordion .accordion-button:not(.collapsed) { color: #202124; background-color: #F8F9FA; border-bottom: 1px solid #E0E0E0; }
  .rendr-accordion .accordion-body { color: #5F6368; font-size: 0.9rem; background-color: #FFFFFF; padding: 1.5rem; }
  
  /* Retain old styles for other tabs */
  .card { border: 1px solid #E0E0E0; border-radius: 12px; box-shadow: 0 1px 3px rgba(0,0,0,.04); margin-bottom: 1rem; }
  .card-header { background: #FFFFFF; color: #202124 !important; border-bottom: 1px solid #E0E0E0; border-radius: 12px 12px 0 0 !important; font-weight: 600; }
  .btn-info { background: #1A73E8; border: none; color: #fff; font-weight: 600; border-radius: 8px; }
  .btn-info:hover { background: #1557B0; color: #fff; }
  .module-about { background: #F8F9FA; border: 1px solid #E8EAED; border-radius: 8px; padding: 1rem; margin: .5rem 0; }
  .module-about h5 { color: #202124; font-weight: 600; }
  .section-hint { background: #F8F9FA; border-left: 4px solid #1A73E8; padding: 10px; font-size: .88rem; }
  
  /* Smart Example Cards Minimal */
  .example-mini-card { border: 1px solid #E0E0E0; border-radius: 8px; padding: 10px; margin-bottom: 8px; transition: all 0.2s; cursor: pointer; background: #FFF; }
  .example-mini-card:hover { border-color: #1A73E8; background: #F8F9FA; }
  .example-mini-card .ex-icon { font-size: 1.2rem; color: #1A73E8; margin-right: 12px; }
  .example-mini-card .ex-title { font-weight: 600; color: #202124; font-size: 0.9rem; }
"

# ═══════════════════════════════════════════════════════════
#  UI
# ═══════════════════════════════════════════════════════════
ui <- page_navbar(
  id = "main_nav",
  title = tags$span(icon("seedling"), tags$strong(" FIP606 Studio")),
  theme = bs_theme(version=5, bg="#FFFFFF", fg="#202124", primary="#1A73E8", secondary="#1A73E8",
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
    downloadButton("dl_report", tags$span(icon("file-word")," Baixar Relatório Completo"), class="btn-warning w-100 mb-2"),
    hr(),
    accordion(open=c("aparencia", "rotulos", "pdf"), multiple=TRUE,
      accordion_panel(title=tags$span(icon("magnifying-glass-plus"), icon("palette"), " Aparência"), value="aparencia",
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
      tags$p(class="rendr-subtitle", "O FIP606 lerá seus dados e sugerirá o gráfico e o teste estatístico adequados. CSV ou Excel (.xlsx)."),
      
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
      
      tags$div(class="rendr-accordion",
        accordion(open = FALSE,
          accordion_panel(title=tags$span(icon("graduation-cap"), " Material das Aulas (FIP606)"), value="aulas",
            tags$p(style="margin-bottom: 15px;", "Baixe aqui os arquivos base (.qmd) utilizados nas aulas da disciplina:"),
            uiOutput("ui_aulas_download")
          ),
          accordion_panel(title="Dados de Exemplo (FIP606)",
            tags$div(class="row",
              tags$div(class="col-md-6",
                tags$div(class="example-mini-card d-flex align-items-center", tags$span(class="ex-icon", icon("magnifying-glass-chart")), tags$div(actionLink("btn_ex_aed", tags$span(class="ex-title", "AED (Mofo)")))),
                tags$div(class="example-mini-card d-flex align-items-center", tags$span(class="ex-icon", icon("scale-balanced")), tags$div(actionLink("btn_ex_ttest", tags$span(class="ex-title", "Teste T (Escala)")))),
                tags$div(class="example-mini-card d-flex align-items-center", tags$span(class="ex-icon", icon("layer-group")), tags$div(actionLink("btn_ex_anova", tags$span(class="ex-title", "ANOVA (Fungicida Vaso)"))))
              ),
              tags$div(class="col-md-6",
                tags$div(class="example-mini-card d-flex align-items-center", tags$span(class="ex-icon", icon("bug")), tags$div(actionLink("btn_ex_glm", tags$span(class="ex-title", "GLM (Insetos)")))),
                tags$div(class="example-mini-card d-flex align-items-center", tags$span(class="ex-icon", icon("chart-line")), tags$div(actionLink("btn_ex_reg", tags$span(class="ex-title", "Regressão (Nitrogênio)")))),
                tags$div(class="example-mini-card d-flex align-items-center", tags$span(class="ex-icon", icon("virus")), tags$div(actionLink("btn_ex_audpc", tags$span(class="ex-title", "AUDPC (Curvas de Progresso)"))))
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
  
  # ─── ABA 0: DADOS (OCULTA INICIALMENTE) ─────────────────
  nav_panel(title=tags$span(icon("table")," Dados"), value="aba_dados",
    layout_sidebar(sidebar=sidebar(width=320,
      tags$h5(icon("wrench"), " Curadoria (Wrangling)"),
      uiOutput("ui_sheet_selector_dados"),
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

  # ─── ABA 1: AED ─────────────────────────────────────────
  nav_panel(title=tags$span(icon("magnifying-glass-chart")," Análise Exploratória de Dados"), value="aba_aed",
    layout_sidebar(sidebar=sidebar(
      uiOutput("ui_aed_x"), uiOutput("ui_aed_y"), uiOutput("ui_aed_color"),
      hr(),
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
      nav_panel(tags$span(icon("chart-bar")," Distribuição Gaussiana"),
        div(class="d-flex justify-content-end p-2 export-btn-group",
          downloadButton("dl_plot_gauss", tags$span(icon("file-pdf")," PDF"), class="btn-sm btn-outline-secondary"),
          downloadButton("dl_plot_gauss_png", tags$span(icon("file-image")," PNG"), class="btn-sm btn-outline-secondary"),
          downloadButton("dl_plot_gauss_svg", tags$span(icon("file-code")," SVG"), class="btn-sm btn-outline-secondary")
        ),
        uiOutput("gauss_shapiro_badge"),
        uiOutput("dyn_plot_gauss")),
      nav_panel(tags$span(icon("circle-info")," Sobre"),
        tags$div(class="module-about", h5(icon("chart-bar")," Análise Exploratória de Dados"),
                 p("Selecione as variáveis para gerar boxplots, dispersões e correlações de Pearson automaticamente."),
                 tags$span("Selecione uma variável Categórica no Eixo X e uma Numérica no Eixo Y para criar um Boxplot. Se escolher duas variáveis Numéricas, o sistema criará um gráfico de Dispersão e calculará automaticamente a correlação de Pearson!")))
    ))
  ),
  
  # ─── ABA 2: TESTE T ────────────────────────────────────
  nav_panel(title=tags$span(icon("scale-balanced")," Teste T"), value="aba_ttest",
    layout_sidebar(sidebar=sidebar(
      uiOutput("ui_ttest_group"), uiOutput("ui_ttest_resp"),
      hr(),
      selectInput("transf_ttest", tags$span(icon("arrows-rotate"), " Transformação (Y):"),
                  choices=transf_choices, selected="Nenhuma"),
      uiOutput("transf_ttest_hint")
    ),

    navset_card_tab(
      nav_panel(tags$span(icon("chart-box")," Boxplot"),
        div(class="d-flex justify-content-end p-2 export-btn-group",
          downloadButton("dl_plot_ttest", tags$span(icon("file-pdf")," PDF"), class="btn-sm btn-outline-secondary"),
          downloadButton("dl_plot_ttest_png", tags$span(icon("file-image")," PNG"), class="btn-sm btn-outline-secondary"),
          downloadButton("dl_plot_ttest_svg", tags$span(icon("file-code")," SVG"), class="btn-sm btn-outline-secondary")
        ),
        uiOutput("dyn_plot_ttest")),
      nav_panel(tags$span(icon("terminal")," Resultados Brutos"),
        div(class="d-flex justify-content-end p-2", downloadButton("dl_res_ttest", tags$span(icon("file-lines")," TXT"), class="btn-sm btn-outline-secondary")),
        verbatimTextOutput("res_ttest")),
      nav_panel(tags$span(icon("lightbulb")," Interpretação"), uiOutput("interp_ttest")),
      nav_panel(tags$span(icon("quote-left")," Texto Científico"), uiOutput("sci_ttest")),
      nav_panel(tags$span(icon("circle-info")," Sobre"),
        tags$div(class="module-about", h5(icon("scale-balanced")," Testes de Hipóteses (T / Wilcoxon)"),
                 p("Compare 2 grupos. O app roda T-Test, Wilcoxon e Shapiro-Wilk automaticamente."),
                 tags$span("A variável de Grupo deve possuir exatamente 2 níveis (ex: Presença/Ausência). O app testará a normalidade pelo teste de Shapiro-Wilk e escolherá automaticamente o Teste T (paramétrico) ou Wilcoxon (não-paramétrico) para exibir o p-valor.")))
    ))
  ),
  
  # ─── ABA 3: ANOVA ──────────────────────────────────────
  nav_panel(title=tags$span(icon("layer-group")," ANOVA"), value="aba_anova",
    layout_sidebar(sidebar=sidebar(
      uiOutput("ui_anova_trat1"), uiOutput("ui_anova_trat2"),
      uiOutput("ui_anova_resp"), uiOutput("ui_anova_bloco"),
      tags$div(class="warn-dic", icon("circle-info"), HTML(' Se <b>"Nenhum"</b> for selecionado no Bloco, o modelo será ajustado como <b>DIC</b>. Se selecionado, será <b>DBC</b>.')),
      hr(),
      radioButtons("anova_method", "Método:", choices=c("Paramétrico (ANOVA)", "Não-Paramétrico (Kruskal/Friedman)")),
      uiOutput("ui_anova_posthoc"),
      hr(),
      selectInput("transf_anova", tags$span(icon("arrows-rotate"), " Transformação (Y):"),
                  choices=transf_choices, selected="Nenhuma"),
      uiOutput("transf_anova_hint")
    ),

    navset_card_tab(
      nav_panel(tags$span(icon("chart-column")," Médias (Tukey)"),
        div(class="d-flex justify-content-end p-2 export-btn-group",
          downloadButton("dl_plot_anova", tags$span(icon("file-pdf")," PDF"), class="btn-sm btn-outline-secondary"),
          downloadButton("dl_plot_anova_png", tags$span(icon("file-image")," PNG"), class="btn-sm btn-outline-secondary"),
          downloadButton("dl_plot_anova_svg", tags$span(icon("file-code")," SVG"), class="btn-sm btn-outline-secondary")
        ),
        uiOutput("dyn_plot_anova_tukey")),
      nav_panel(tags$span(icon("terminal")," Resultados Brutos"),
        div(class="d-flex justify-content-end p-2", downloadButton("dl_res_anova", tags$span(icon("file-lines")," TXT"), class="btn-sm btn-outline-secondary")),
        verbatimTextOutput("res_anova")),
      nav_panel(tags$span(icon("stethoscope")," Diagnóstico DHARMa"), withSpinner(plotOutput("plot_anova_dharma", height="480px"), type=6, color="#18BC9C")),
      nav_panel(tags$span(icon("lightbulb")," Interpretação"), uiOutput("interp_anova")),
      nav_panel(tags$span(icon("quote-left")," Texto Científico"), uiOutput("sci_anova")),
      nav_panel(tags$span(icon("circle-info")," Sobre"),
        tags$div(class="module-about", h5(icon("layer-group")," ANOVA — Fatorial e Blocos"),
                 p("Defina os fatores, a resposta e o bloco. O app gera ANOVA, médias Tukey e DHARMa."),
                 tags$span(HTML("<b>Tratamento 1:</b> Fator principal. <b>Tratamento 2:</b> Deixe em 'Nenhum' para ANOVA simples ou adicione para Fatorial Duplo. <b>Bloco:</b> Adiciona o fator bloco ao modelo (DBC). O diagnóstico DHARMa ajuda a validar a adequação dos resíduos."))))
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
      tags$div(class="warn-dic", icon("circle-info"),
        HTML(' <b>Poisson:</b> Contagens sem sobredispersão. <b>Quasipoisson:</b> Corrige sobredispersão. <b>Binomial Negativa:</b> Alternativa formal para sobredispersão.'))
    ),

    navset_card_tab(
      nav_panel(tags$span(icon("chart-column")," Taxas Estimadas"),
        div(class="d-flex justify-content-end p-2 export-btn-group",
          downloadButton("dl_plot_glm", tags$span(icon("file-pdf")," PDF"), class="btn-sm btn-outline-secondary"),
          downloadButton("dl_plot_glm_png", tags$span(icon("file-image")," PNG"), class="btn-sm btn-outline-secondary"),
          downloadButton("dl_plot_glm_svg", tags$span(icon("file-code")," SVG"), class="btn-sm btn-outline-secondary")
        ),
        uiOutput("dyn_plot_glm_medias")),
      nav_panel(tags$span(icon("terminal")," Resultados Brutos"),
        div(class="d-flex justify-content-end p-2", downloadButton("dl_res_glm", tags$span(icon("file-lines")," TXT"), class="btn-sm btn-outline-secondary")),
        verbatimTextOutput("res_glm")),
      nav_panel(tags$span(icon("stethoscope")," Diagnóstico DHARMa"), withSpinner(plotOutput("plot_glm_dharma", height="480px"), type=6, color="#18BC9C")),
      nav_panel(tags$span(icon("lightbulb")," Interpretação"), uiOutput("interp_glm")),
      nav_panel(tags$span(icon("quote-left")," Texto Científico"), uiOutput("sci_glm")),
      nav_panel(tags$span(icon("circle-info")," Sobre"),
        tags$div(class="module-about", h5(icon("bug")," Modelos Lineares Generalizados (GLM)"),
                 p("Para dados discretos (contagens). Suporta Poisson, Quasipoisson e Binomial Negativa."),
                 tags$span(HTML("<b>Poisson:</b> Modelo padrão para contagens. <b>Quasipoisson:</b> Ajusta erros-padrão quando há sobredispersão. <b>Binomial Negativa:</b> Alternativa formal com parâmetro extra de dispersão via MASS::glm.nb(). Use <b>Offset</b> para padronizar contagens por unidade (ex: insetos/planta). O <b>Tratamento 2</b> permite análise fatorial."))))
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

    navset_card_tab(
      nav_panel(tags$span(icon("bezier-curve")," Curva Ajustada"),
        div(class="d-flex justify-content-end p-2 export-btn-group",
          downloadButton("dl_plot_reg", tags$span(icon("file-pdf")," PDF"), class="btn-sm btn-outline-secondary"),
          downloadButton("dl_plot_reg_png", tags$span(icon("file-image")," PNG"), class="btn-sm btn-outline-secondary"),
          downloadButton("dl_plot_reg_svg", tags$span(icon("file-code")," SVG"), class="btn-sm btn-outline-secondary")
        ),
        uiOutput("dyn_plot_reg")),
      nav_panel(tags$span(icon("terminal")," Resultados Brutos"),
        div(class="d-flex justify-content-end p-2", downloadButton("dl_res_reg", tags$span(icon("file-lines")," TXT"), class="btn-sm btn-outline-secondary")),
        verbatimTextOutput("res_reg")),
      nav_panel(tags$span(icon("lightbulb")," Interpretação"), uiOutput("interp_reg")),
      nav_panel(tags$span(icon("quote-left")," Texto Científico"), uiOutput("sci_reg")),
      nav_panel(tags$span(icon("circle-info")," Sobre"),
        tags$div(class="module-about", h5(icon("chart-line")," Regressão Linear e Polinomial"),
                 p("Ajuste modelos aos seus dados contínuos. Na quadrática, o ponto ótimo é calculado."),
                 tags$span("As variáveis X e Y precisam ser contínuas numéricas. Na regressão quadrática, o sistema calcula e interpreta automaticamente o ponto de máximo (ou mínimo) da curva parabólica ajustada.")))
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

    navset_card_tab(
      nav_panel(tags$span(icon("chart-area")," Curva de Progresso (Área)"),
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
        div(class="d-flex justify-content-end p-2", downloadButton("dl_res_audpc", tags$span(icon("file-lines")," TXT"), class="btn-sm btn-outline-secondary")),
        verbatimTextOutput("res_audpc")),
      nav_panel(tags$span(icon("lightbulb")," Interpretação"), uiOutput("interp_audpc")),
      nav_panel(tags$span(icon("quote-left")," Texto Científico"), uiOutput("sci_audpc")),
      nav_panel(tags$span(icon("circle-info")," Sobre"),
        tags$div(class="module-about", h5(icon("virus")," AUDPC — Área Abaixo da Curva de Progresso"),
                 p("Calcule e visualize a AUDPC. A área sombreada no gráfico representa a integral trapezoidal."),
                 tags$span(HTML("Tempo e Severidade devem ser colunas numéricas. Para o modelo Epidemiológico (Epifitter), o R ajusta linearizações da curva para estimar a Taxa de Progresso (<i>r</i>) e a Severidade Inicial (<i>y0</i>). Se 'rAUDPC' estiver ativado, a AUDPC calculada será relativa (dividida pela amplitude do tempo)."))))
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
  nav_hide("main_nav", "aba_reg")
  nav_hide("main_nav", "aba_audpc")
  
  rv <- reactiveValues(
    raw_data=NULL, data=NULL, uploaded_file_path=NULL, uploaded_file_ext=NULL,
    aed_x=NULL, aed_y=NULL, aed_color="Nenhum",
    ttest_group=NULL, ttest_resp=NULL,
    anova_trat1=NULL, anova_trat2="Nenhum", anova_resp=NULL, anova_bloco="Nenhum",
    glm_trat=NULL, glm_trat2="Nenhum", glm_resp=NULL, glm_offset="Nenhum", reg_x=NULL, reg_y=NULL,
    audpc_time=NULL, audpc_sev=NULL, audpc_trat=NULL, audpc_rep=NULL
  )
  export <- reactiveValues(plot_aed=NULL,plot_gauss=NULL,plot_ttest=NULL,plot_anova=NULL,plot_glm=NULL,plot_reg=NULL,
                           plot_audpc_curve=NULL,plot_audpc_bar=NULL,plot_epi=NULL,
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
      nav_show("main_nav", "aba_reg")
      nav_show("main_nav", "aba_audpc")
    } else {
      nav_hide("main_nav", "aba_dados")
      nav_hide("main_nav", "aba_aed")
      nav_hide("main_nav", "aba_ttest")
      nav_hide("main_nav", "aba_anova")
      nav_hide("main_nav", "aba_glm")
      nav_hide("main_nav", "aba_reg")
      nav_hide("main_nav", "aba_audpc")
    }
  })
  
  # Toggle da barra lateral baseado na aba ativa
  observeEvent(input$main_nav, {
    if (input$main_nav %in% c("aba_inicio", "aba_dados")) {
      sidebar_toggle("global_sidebar", open = FALSE)
    } else {
      sidebar_toggle("global_sidebar", open = TRUE)
    }
  })
  
  # ─── Dynamic Plotly Renderers ─────────────────────────────
  render_dyn_plot <- function(id, h) {
    if(input$use_plotly) plotlyOutput(paste0("plotly_", id), height=h) else withSpinner(plotOutput(id, height=h), type=6, color="#18BC9C")
  }
  
  # ─── Gerador de Relatório RMarkdown ─────────────────────
  output$dl_report <- downloadHandler(
    filename = function() { paste0("Relatorio_FIP606_", format(Sys.time(), "%Y%m%d_%H%M"), ".docx") },
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
      
      params <- list(
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
        raw_results = list(
          ttest = export$res_ttest %||% NULL,
          anova = export$res_anova %||% NULL,
          glm   = export$res_glm %||% NULL,
          reg   = export$res_reg %||% NULL,
          audpc = export$res_audpc %||% NULL
        ),
        dataset = rv$data,
        file_info = if(is.null(rv$uploaded_file_path)) "Conjunto de Dados de Exemplo Embutido" else "Arquivo de Usuário (Personalizado)"
      )
      
      tryCatch({
        rmarkdown::render(tempReport, output_file = file, params = params, envir = new.env(parent = globalenv()))
      }, error = function(e) {
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
  
  # ─── Helpers reativos para personalizacao ────────────────
  cur_theme   <- reactive({ get_theme_func(input$custom_theme %||% "Clássico") })
  cur_palette <- reactive({ get_palette_colors(input$custom_palette %||% "FIP606 (Padrão)") })
  cur_fsize   <- reactive({ input$custom_font_size %||% 14 })
  cur_alpha   <- reactive({ input$custom_alpha %||% 0.65 })
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
  
  # ─── Exemplos FIP606 ───────────────────────────────────
  observeEvent(input$btn_ex_aed,{ rv$uploaded_file_path<-NULL; d<-read_excel("data/dados_diversos.xlsx","mofo"); rv$raw_data<-d; rv$data<-d; rv$aed_x<-"inc";rv$aed_y<-"yld"; nav_select("main_nav", "aba_aed"); showNotification("Dados de exemplo carregados!", type="message", duration=4) })
  observeEvent(input$btn_ex_ttest,{ rv$uploaded_file_path<-NULL; d<-read_excel("data/dados_diversos.xlsx","escala")|>filter(assessment%in%c("Unaided","Aided1")); rv$raw_data<-d; rv$data<-d; rv$ttest_group<-"assessment";rv$ttest_resp<-"acuracia"; nav_select("main_nav", "aba_ttest"); showNotification("Dados de exemplo carregados!", type="message", duration=4) })
  observeEvent(input$btn_ex_anova,{ rv$uploaded_file_path<-NULL; d<-read_excel("data/dados_diversos.xlsx","fungicida_vaso")|>mutate(incidencia=inf_seeds/n_seeds,dose=factor(dose)); rv$raw_data<-d; rv$data<-d; rv$anova_trat1<-"treat";rv$anova_trat2<-"dose";rv$anova_resp<-"severity";rv$anova_bloco<-"Nenhum"; nav_select("main_nav", "aba_anova"); showNotification("Dados de exemplo carregados!", type="message", duration=4) })
  observeEvent(input$btn_ex_glm,{ rv$uploaded_file_path<-NULL; d<-InsectSprays; rv$raw_data<-d; rv$data<-d; rv$glm_trat<-"spray";rv$glm_resp<-"count"; nav_select("main_nav", "aba_glm"); showNotification("Dados de exemplo carregados!", type="message", duration=4) })
  observeEvent(input$btn_ex_reg,{ rv$uploaded_file_path<-NULL; d<-data.frame(DOSEN=c(0,50,100,150,200,250),RG=c(7.1,7.3,7.66,7.71,7.62,7.6)); rv$raw_data<-d; rv$data<-d; rv$reg_x<-"DOSEN";rv$reg_y<-"RG"; nav_select("main_nav", "aba_reg"); showNotification("Dados de exemplo carregados!", type="message", duration=4) })
  observeEvent(input$btn_ex_audpc,{ rv$uploaded_file_path<-NULL; d<-read_excel("data/dados_diversos.xlsx","curve"); rv$raw_data<-d; rv$data<-d; rv$audpc_time<-"day";rv$audpc_sev<-"severity";rv$audpc_trat<-"Irrigation";rv$audpc_rep<-"rep"; nav_select("main_nav", "aba_audpc"); showNotification("Dados de exemplo carregados!", type="message", duration=4) })
  
  observeEvent(input$aed_x,{rv$aed_x<-input$aed_x}); observeEvent(input$aed_y,{rv$aed_y<-input$aed_y}); observeEvent(input$aed_color,{rv$aed_color<-input$aed_color})
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
  output$ui_aed_x<-renderUI(selectInput("aed_x","Eixo X:",col_names(),selected=rv$aed_x))
  output$ui_aed_y<-renderUI(selectInput("aed_y","Eixo Y:",num_cols(),selected=rv$aed_y))
  output$ui_aed_color<-renderUI(selectInput("aed_color","Cor:",c("Nenhum",col_names()),selected=rv$aed_color))
  output$ui_ttest_group<-renderUI(selectInput("ttest_group","Grupo (2 níveis):",col_names(),selected=rv$ttest_group))
  output$ui_ttest_resp<-renderUI(selectInput("ttest_resp","Resposta:",num_cols(),selected=rv$ttest_resp))
  output$ui_anova_trat1<-renderUI(selectInput("anova_trat1","Tratamento 1:",col_names(),selected=rv$anova_trat1))
  output$ui_anova_trat2<-renderUI(selectInput("anova_trat2","Tratamento 2 (Fatorial):",c("Nenhum",col_names()),selected=rv$anova_trat2))
  output$ui_anova_resp<-renderUI(selectInput("anova_resp","Resposta:",num_cols(),selected=rv$anova_resp))
  output$ui_anova_bloco<-renderUI(selectInput("anova_bloco","Bloco:",c("Nenhum",col_names()),selected=rv$anova_bloco))
  
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
  
  # ══════════════════════════════════════════════════════════
  #  TAB 0 - DADOS BRUTOS
  # ══════════════════════════════════════════════════════════
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
    }, error = function(e) {
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
      if (is.numeric(col)) {
        col_clean <- col[!is.na(col)]
        data.frame(
          Coluna = col_name,
          Tipo = "Numérica",
          n = length(col_clean),
          Média = round(mean(col_clean), 3),
          DP = round(sd(col_clean), 3),
          Mín = round(min(col_clean), 3),
          Mediana = round(median(col_clean), 3),
          Máx = round(max(col_clean), 3),
          stringsAsFactors = FALSE
        )
      } else {
        col_fac <- as.factor(col)
        tab <- table(col_fac)
        moda <- if(length(tab) > 0) names(tab)[which.max(tab)] else NA
        data.frame(
          Coluna = col_name,
          Tipo = "Fator",
          n = sum(!is.na(col)),
          Média = NA,
          DP = NA,
          Mín = NA,
          Mediana = NA,
          Máx = NA,
          stringsAsFactors = FALSE
        )
      }
    })
    do.call(rbind, stats_list)
  })
  
  output$tabela_desc_stats <- renderDT({
    req(desc_stats())
    datatable(desc_stats(), options=list(pageLength=20, scrollX=TRUE, dom="t"), rownames=FALSE, class="compact stripe hover")
  })

  # ══════════════════════════════════════════════════════════
  #  TAB 1 - AED
  # ══════════════════════════════════════════════════════════
  output$plot_aed <- renderPlot({
    req(rv$data, rv$aed_x, rv$aed_y)
    pal <- cur_palette(); fsize <- cur_fsize(); alpha_val <- cur_alpha(); leg_pos <- cur_legend()
    theme_fn <- cur_theme(); tt <- input$transf_aed %||% "Nenhuma"
    
    df <- rv$data
    df <- apply_plot_levels(df, rv$aed_x, input$custom_levels_1, rv$aed_color, input$custom_levels_2)
    ylabel <- transf_label(rv$aed_y, tt)
    if (tt != "Nenhuma") { df[["y_transf"]] <- apply_transform(df[[rv$aed_y]], tt) } else { df[["y_transf"]] <- df[[rv$aed_y]] }
    
    p <- ggplot(df, aes(x=!!sym(rv$aed_x), y=y_transf))
    
    if (is.numeric(rv$data[[rv$aed_x]])) {
      if (rv$aed_color != "Nenhum") {
        if (cur_jitter()) p <- p + geom_point(aes(color=!!sym(rv$aed_color)), size=cur_pt_size(), alpha=alpha_val)
        p <- p + geom_smooth(method="lm", se=FALSE, aes(color=!!sym(rv$aed_color))) +
          stat_cor(aes(color=!!sym(rv$aed_color)), method="pearson", show.legend=FALSE, size=4) +
          scale_color_manual(values=pal)
      } else {
        if (cur_jitter()) p <- p + geom_point(size=cur_pt_size(), color=pal[1], alpha=alpha_val)
        p <- p + geom_smooth(method="lm", se=FALSE, color=pal[2]) +
          stat_cor(method="pearson", size=4)
      }
    } else {
      if (rv$aed_color != "Nenhum") {
        p <- p + geom_boxplot(aes(fill=!!sym(rv$aed_color)), outlier.color=NA, alpha=alpha_val)
        if (cur_jitter()) p <- p + geom_jitter(width=.15, alpha=alpha_val*0.7, size=cur_pt_size(), aes(color=!!sym(rv$aed_color)))
        p <- p + scale_fill_manual(values=pal) + scale_color_manual(values=pal)
      } else {
        p <- p + geom_boxplot(fill=pal[2], outlier.color=NA, alpha=alpha_val)
        if (cur_jitter()) p <- p + geom_jitter(width=.15, alpha=alpha_val*0.8, size=cur_pt_size(), color=pal[1])
      }
    }
    
    p <- p + theme_fn(base_size=fsize) +
      theme(plot.title=element_text(face="bold"), legend.position=leg_pos) +
      labs(y=ylabel)
    p <- apply_custom_labels(p, input)
    export$plot_aed <- p; p
  })
  
  output$dl_plot_aed     <- downloadHandler("AED_plot.pdf", function(f) ggsave(f, export$plot_aed, device="pdf", width=cur_pdf_w(), height=cur_pdf_h()))
  output$dl_plot_aed_png <- downloadHandler("AED_plot.png", function(f) ggsave(f, export$plot_aed, device="png", width=cur_pdf_w(), height=cur_pdf_h(), dpi=300))
  output$dl_plot_aed_svg <- downloadHandler("AED_plot.svg", function(f) ggsave(f, export$plot_aed, device="svg", width=cur_pdf_w(), height=cur_pdf_h()))

  # ── Distribuição Gaussiana ─────────────────────────────────
  output$plot_gauss <- renderPlot({
    req(rv$data, rv$aed_y)
    pal <- cur_palette(); fsize <- cur_fsize(); alpha_val <- cur_alpha(); leg_pos <- cur_legend()
    theme_fn <- cur_theme(); tt <- input$transf_aed %||% "Nenhuma"
    
    df <- rv$data
    ylabel <- transf_label(rv$aed_y, tt)
    if (tt != "Nenhuma") { y_vals <- apply_transform(df[[rv$aed_y]], tt) } else { y_vals <- df[[rv$aed_y]] }
    y_vals <- y_vals[!is.na(y_vals)]
    req(length(y_vals) >= 3)
    
    df_plot <- data.frame(y = y_vals)
    mu <- mean(y_vals)
    sigma <- sd(y_vals)
    
    p <- ggplot(df_plot, aes(x = y)) +
      geom_histogram(aes(y = after_stat(density)), bins = 30,
                     fill = pal[2], color = "white", alpha = alpha_val) +
      stat_function(fun = dnorm, args = list(mean = mu, sd = sigma),
                    color = pal[1], linewidth = 1.2, linetype = "solid") +
      geom_vline(xintercept = mu, color = pal[min(3, length(pal))],
                 linewidth = 0.8, linetype = "dashed") +
      annotate("text", x = mu, y = Inf,
               label = paste0("\u03bc = ", round(mu, 3), "\n\u03c3 = ", round(sigma, 3)),
               vjust = 1.5, hjust = -0.1, size = 4.5, fontface = "italic", color = pal[1]) +
      theme_fn(base_size = fsize) +
      theme(plot.title = element_text(face = "bold"), legend.position = leg_pos) +
      labs(x = ylabel, y = "Densidade",
           title = paste0("Distribui\u00e7\u00e3o de ", ylabel, " com curva Normal ajustada"))
    p <- apply_custom_labels(p, input)
    export$plot_gauss <- p; p
  })
  
  output$gauss_shapiro_badge <- renderUI({
    req(rv$data, rv$aed_y)
    tt <- input$transf_aed %||% "Nenhuma"
    df <- rv$data
    if (tt != "Nenhuma") { y_vals <- apply_transform(df[[rv$aed_y]], tt) } else { y_vals <- df[[rv$aed_y]] }
    y_vals <- y_vals[!is.na(y_vals)]
    if (length(y_vals) < 3 || length(y_vals) > 5000) {
      return(tags$div(class = "alert alert-warning m-2",
             icon("triangle-exclamation"), " Shapiro-Wilk requer entre 3 e 5000 observa\u00e7\u00f5es."))
    }
    sw <- shapiro.test(y_vals)
    pval <- sw$p.value
    if (pval > 0.05) {
      badge_class <- "alert-success"
      icn <- icon("circle-check")
      msg <- paste0("Teste de Shapiro-Wilk: W = ", round(sw$statistic, 4),
                    ", p = ", format(pval, digits = 4),
                    " \u2014 N\u00e3o rejeita normalidade (p > 0.05). Os dados seguem distribui\u00e7\u00e3o aproximadamente normal.")
    } else {
      badge_class <- "alert-danger"
      icn <- icon("circle-xmark")
      msg <- paste0("Teste de Shapiro-Wilk: W = ", round(sw$statistic, 4),
                    ", p = ", format(pval, digits = 4),
                    " \u2014 Rejeita normalidade (p \u2264 0.05). Os dados N\u00c3O seguem distribui\u00e7\u00e3o normal.")
    }
    tags$div(class = paste("alert", badge_class, "m-2 d-flex align-items-center gap-2"), icn, msg)
  })
  
  output$dl_plot_gauss     <- downloadHandler("Gaussiana_plot.pdf", function(f) ggsave(f, export$plot_gauss, device="pdf", width=cur_pdf_w(), height=cur_pdf_h()))
  output$dl_plot_gauss_png <- downloadHandler("Gaussiana_plot.png", function(f) ggsave(f, export$plot_gauss, device="png", width=cur_pdf_w(), height=cur_pdf_h(), dpi=300))
  output$dl_plot_gauss_svg <- downloadHandler("Gaussiana_plot.svg", function(f) ggsave(f, export$plot_gauss, device="svg", width=cur_pdf_w(), height=cur_pdf_h()))
  
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
  
  output$plot_ttest <- renderPlot({
    req(ttest_data())
    df <- ttest_data()
    df <- apply_plot_levels(df, rv$ttest_group, input$custom_levels_1)
    pal <- cur_palette(); fsize <- cur_fsize(); alpha_val <- cur_alpha(); leg_pos <- cur_legend()
    theme_fn <- cur_theme(); tt <- input$transf_ttest %||% "Nenhuma"
    ylabel <- transf_label(rv$ttest_resp, tt)
    
    p <- ggplot(df, aes(x=!!sym(rv$ttest_group), y=resp_transf, fill=!!sym(rv$ttest_group))) +
      geom_boxplot(outlier.color=NA, alpha=alpha_val, width=.5)
    if (cur_jitter()) p <- p + geom_jitter(width=.12, alpha=alpha_val*0.7, size=cur_pt_size())
    p <- p + scale_fill_manual(values=pal) +
      theme_fn(base_size=fsize) +
      theme(legend.position="none", plot.title=element_text(face="bold")) +
      labs(title="Comparação entre Grupos", x=rv$ttest_group, y=ylabel)
    p <- apply_custom_labels(p, input)
    export$plot_ttest <- p; p
  })
  
  output$res_ttest <- renderPrint({
    req(ttest_data())
    df <- ttest_data()
    niveis <- unique(na.omit(df[[rv$ttest_group]])); if(length(niveis)!=2) return(cat("Requer 2 níveis."))
    tt <- input$transf_ttest %||% "Nenhuma"
    f <- as.formula(paste("resp_transf", "~", rv$ttest_group))
    out <- capture.output({
      if(tt!="Nenhuma") cat(paste0("*** Transformação aplicada: ", tt, " ***\n\n"))
      cat("=== T-Test (var.equal=TRUE) ===\n"); print(t.test(f, data=df, var.equal=TRUE))
      cat("\n=== Wilcoxon ===\n"); print(wilcox.test(f, data=df))
      cat("\n=== Shapiro-Wilk ===\n"); print(shapiro.test(lm(f, data=df)$residuals))
    })
    export$res_ttest <- out; cat(out, sep="\n")
  })
  
  output$interp_ttest <- renderUI({
    req(ttest_data())
    df <- ttest_data()
    niveis <- unique(na.omit(df[[rv$ttest_group]])); if(length(niveis)!=2) return(tags$div(class="interp-box","Selecione um grupo com 2 níveis."))
    tt <- input$transf_ttest %||% "Nenhuma"
    f <- as.formula(paste("resp_transf", "~", rv$ttest_group))
    t_res <- t.test(f, data=df, var.equal=TRUE); wt <- wilcox.test(f, data=df)
    sh <- shapiro.test(lm(f, data=df)$residuals)
    medias <- df |> group_by(across(all_of(rv$ttest_group))) |> summarise(m=mean(resp_transf, na.rm=TRUE), .groups="drop")
    g1 <- medias[[rv$ttest_group]][1]; g2 <- medias[[rv$ttest_group]][2]
    m1 <- round(medias$m[1],2); m2 <- round(medias$m[2],2)
    normal_txt <- if(sh$p.value>0.05) paste0("Os resíduos <b>seguem distribuição normal</b> (Shapiro p=",round(sh$p.value,4),"), portanto o <b>Teste T</b> é adequado.") else paste0("Os resíduos <b>NÃO seguem distribuição normal</b> (Shapiro p=",round(sh$p.value,4),"), portanto o <b>Teste de Wilcoxon</b> é mais apropriado.")
    teste_usado <- if(sh$p.value>0.05) t_res else wt; p_usado <- teste_usado$p.value
    concl <- if(p_usado<=0.05) paste0("Existe <b>diferença estatisticamente significativa</b> entre os grupos <b>",g1,"</b> (média=",m1,") e <b>",g2,"</b> (média=",m2,").") else paste0("Não há diferença estatisticamente significativa entre os grupos <b>",g1,"</b> (média=",m1,") e <b>",g2,"</b> (média=",m2,").")
    
    items <- list()
    if (tt != "Nenhuma") items <- c(items, list(tags$div(class="interp-item", HTML(paste0("<b>Transformação:</b> ", tt, " aplicada à variável resposta.")))))
    items <- c(items, list(
      tags$div(class="interp-item", HTML(paste0("<b>1. Normalidade:</b> ", normal_txt))),
      tags$div(class="interp-item", HTML(paste0("<b>2. Significância:</b> ", badge_sig(p_usado)))),
      tags$div(class="interp-item", HTML(paste0("<b>3. Conclusão:</b> ", concl)))
    ))
    tags$div(class="interp-box", h5(icon("lightbulb")," Interpretação dos Resultados"), tagList(items))
  })
  
  output$sci_ttest <- renderUI({
    req(ttest_data())
    df <- ttest_data()
    f <- as.formula(paste("resp_transf", "~", rv$ttest_group))
    t_res <- t.test(f, data=df, var.equal=TRUE)
    wt <- wilcox.test(f, data=df)
    sh <- shapiro.test(lm(f, data=df)$residuals)
    medias <- df |> group_by(across(all_of(rv$ttest_group))) |> summarise(m=mean(resp_transf, na.rm=TRUE), .groups="drop")
    
    g1 <- medias[[rv$ttest_group]][1]; g2 <- medias[[rv$ttest_group]][2]
    m1 <- round(medias$m[1],2); m2 <- round(medias$m[2],2)
    
    if (sh$p.value > 0.05) {
      txt <- sprintf("Os dados foram submetidos ao Teste T para amostras independentes, visto que a premissa de normalidade dos resíduos foi atendida (Shapiro-Wilk, W = %.3f, p = %.3f). O teste indicou que %s diferença estatisticamente significativa entre os tratamentos %s (Média = %.2f) e %s (Média = %.2f) (t = %.2f, df = %.1f, p %s).",
                     sh$statistic, sh$p.value,
                     if(t_res$p.value <= 0.05) "houve" else "não houve",
                     g1, m1, g2, m2,
                     t_res$statistic, t_res$parameter,
                     if(t_res$p.value < 0.001) "< 0.001" else sprintf("= %.3f", t_res$p.value))
    } else {
      txt <- sprintf("Devido à violação da premissa de normalidade dos resíduos (Shapiro-Wilk, W = %.3f, p = %.3f), os dados foram submetidos ao Teste Não-Paramétrico de Wilcoxon-Mann-Whitney. Os resultados revelaram que %s diferença significativa entre os tratamentos %s e %s (W = %.1f, p %s).",
                     sh$statistic, sh$p.value,
                     if(wt$p.value <= 0.05) "houve" else "não houve",
                     g1, g2,
                     wt$statistic,
                     if(wt$p.value < 0.001) "< 0.001" else sprintf("= %.3f", wt$p.value))
    }
    
    export$txt_ttest <- txt
    tags$div(class="interp-box", h5(icon("quote-left")," Texto Sugerido para Publicação"), p(txt),
             tags$small(class="text-muted", "Nota: Revise o texto antes de publicá-lo para garantir que a terminologia se adequa à sua área de estudo."))
  })
  
  output$dl_plot_ttest     <- downloadHandler("ttest_plot.pdf", function(f) ggsave(f, export$plot_ttest, device="pdf", width=cur_pdf_w(), height=cur_pdf_h()))
  output$dl_plot_ttest_png <- downloadHandler("ttest_plot.png", function(f) ggsave(f, export$plot_ttest, device="png", width=cur_pdf_w(), height=cur_pdf_h(), dpi=300))
  output$dl_plot_ttest_svg <- downloadHandler("ttest_plot.svg", function(f) ggsave(f, export$plot_ttest, device="svg", width=cur_pdf_w(), height=cur_pdf_h()))
  output$dl_res_ttest  <- downloadHandler("ttest_res.txt",  function(f) writeLines(export$res_ttest, f))
  output$dl_report_ttest <- downloadHandler("ttest_report.txt", function(f) writeLines(export$txt_ttest, f))
  
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
    if (rv$anova_bloco != "Nenhum") { df[[rv$anova_bloco]] <- as.factor(df[[rv$anova_bloco]]); ef <- paste(ef, "+", rv$anova_bloco) }
    lm(as.formula(paste("resp_transf", "~", ef)), data=df)
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
      
      p <- ggplot(df_plot, aes(x=!!sym(rv$anova_trat1), y=resp_transf, fill=!!sym(rv$anova_trat1))) +
        geom_boxplot(alpha=alpha_val, outlier.color=NA)
      if (cur_jitter()) p <- p + geom_jitter(width=0.15, alpha=alpha_val*0.8, size=cur_pt_size(), color="gray30")
      p <- p + geom_text(data=letras_df, aes(x=!!sym(rv$anova_trat1), y=max_val, label=.group), vjust=-0.8, fontface="bold", size=5) +
        scale_fill_manual(values=pal) + theme_fn(base_size=fsize) +
        theme(plot.title=element_text(face="bold"), legend.position="none") +
        labs(title="Boxplot e Teste de Wilcoxon (Holm)", y=paste("Mediana —", ylabel))
      p <- apply_custom_labels(p, input)
      export$plot_anova <- p; return(p)
    }
    
    if (rv$anova_trat2 != "Nenhum") {
      letras_df <- clean_cld(emmeans(m, as.formula(paste("~", rv$anova_trat1, "|", rv$anova_trat2))))
      letras_df <- apply_plot_levels(letras_df, rv$anova_trat1, input$custom_levels_1, rv$anova_trat2, input$custom_levels_2)
      p <- ggplot(letras_df, aes(x=!!sym(rv$anova_trat1), y=emmean, fill=!!sym(rv$anova_trat2))) +
        geom_bar(stat="identity", position=position_dodge(.8), width=.7, color="black", alpha=alpha_val) +
        geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), position=position_dodge(.8), width=.2) +
        geom_text(aes(y=emmean+SE, label=.group), position=position_dodge(.8), vjust=-.5, fontface="bold", size=4) +
        scale_fill_manual(values=pal) +
        theme_fn(base_size=fsize) +
        theme(plot.title=element_text(face="bold"), legend.position=leg_pos) +
        labs(y=paste("Média Estimada —", ylabel), title="Interação Fatorial (Tukey)")
    } else {
      letras_df <- clean_cld(emmeans(m, as.formula(paste("~", rv$anova_trat1))))
      df_plot <- df; df_plot[[rv$anova_trat1]] <- as.factor(df_plot[[rv$anova_trat1]])
      if (tt != "Nenhuma") { df_plot[["resp_transf"]] <- apply_transform(df_plot[[rv$anova_resp]], tt) } else { df_plot[["resp_transf"]] <- df_plot[[rv$anova_resp]] }
      
      letras_df <- apply_plot_levels(letras_df, rv$anova_trat1, input$custom_levels_1)
      df_plot <- apply_plot_levels(df_plot, rv$anova_trat1, input$custom_levels_1)
      
      p <- ggplot()
      if (cur_jitter()) p <- p + geom_jitter(data=df_plot, aes(x=!!sym(rv$anova_trat1), y=resp_transf), width=.12, alpha=alpha_val*0.5, size=cur_pt_size(), color="gray50")
      p <- p + geom_errorbar(data=letras_df, aes(x=!!sym(rv$anova_trat1), ymin=lower.CL, ymax=upper.CL), width=.2, linewidth=.8) +
        geom_point(data=letras_df, aes(x=!!sym(rv$anova_trat1), y=emmean), size=3.5, color=pal[3]) +
        geom_text(data=letras_df, aes(x=!!sym(rv$anova_trat1), y=upper.CL, label=.group), vjust=-1, fontface="bold", size=5) +
        theme_fn(base_size=fsize) +
        theme(plot.title=element_text(face="bold"), legend.position=leg_pos) +
        labs(title="Médias Ajustadas (Tukey)", y=paste("Média Estimada —", ylabel))
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
          ft <- suppressWarnings(friedman.test(df[["resp_transf"]], df[[rv$anova_trat1]], df[[rv$anova_bloco]]))
          print(ft)
        } else {
          cat("Teste de Kruskal-Wallis (DIC):\n")
          kt <- kruskal.test(df[["resp_transf"]], df[[rv$anova_trat1]])
          print(kt)
        }
        cat("\n=== Comparações Múltiplas (Wilcoxon pareado com ajuste Holm) ===\n")
        print(suppressWarnings(pairwise.wilcox.test(df[["resp_transf"]], df[[rv$anova_trat1]], p.adjust.method="holm", exact=FALSE)))
      }
    })
    export$res_anova <- out; cat(out, sep="\n")
  })
  
  output$plot_anova_dharma <- renderPlot({ 
    if(input$anova_method != "Paramétrico (ANOVA)") return(plot(1, type="n", axes=FALSE, xlab="", ylab="", main="DHARMa não se aplica a testes não-paramétricos."))
    req(modelo_anova()); plot(simulateResiduals(modelo_anova())) 
  })
  
  output$interp_anova <- renderUI({
    req(modelo_anova())
    if (input$anova_method != "Paramétrico (ANOVA)") return(tags$div(class="interp-box", h5(icon("lightbulb")," Interpretação Não-Paramétrica"), HTML("Você selecionou o teste não-paramétrico. Acesse a aba <b>Texto Científico</b> para uma descrição dos resultados e veja o gráfico com o agrupamento do teste de Wilcoxon.")))
    
    m <- modelo_anova(); av <- anova(m)
    tt <- input$transf_anova %||% "Nenhuma"
    delineamento <- if(rv$anova_bloco=="Nenhum") "DIC (Delineamento Inteiramente Casualizado)" else "DBC (Delineamento em Blocos Casualizados)"
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
      df1 <- av[rv$anova_trat1, "Df"]; df2 <- av["Residuals", "Df"]
      txt <- sprintf("Os dados foram submetidos à Análise de Variância (ANOVA). O efeito do tratamento principal sobre a variável resposta foi %s (F(%d, %d) = %.2f, p %s). %s",
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
    resp_type <- if(is_nb) "response" else "response"
    
    if (rv$glm_trat2 != "Nenhum" && rv$glm_trat2 %in% names(rv$data)) {
      em_fml <- as.formula(paste("~", rv$glm_trat, "|", rv$glm_trat2))
      letras_df <- clean_cld(emmeans(m, em_fml, type=resp_type))
      
      rate_col <- if("rate" %in% names(letras_df)) "rate" else if("response" %in% names(letras_df)) "response" else "emmean"
      se_col <- if("SE" %in% names(letras_df)) "SE" else "SE"
      
      letras_df <- apply_plot_levels(letras_df, rv$glm_trat, input$custom_levels_1, rv$glm_trat2, input$custom_levels_2)
      
      p <- ggplot(letras_df, aes(x=!!sym(rv$glm_trat), y=!!sym(rate_col), fill=!!sym(rv$glm_trat2))) +
        geom_bar(stat="identity", position=position_dodge(.8), width=.7, color="black", alpha=alpha_val) +
        geom_errorbar(aes(ymin=!!sym(rate_col)-!!sym(se_col), ymax=!!sym(rate_col)+!!sym(se_col)),
                      position=position_dodge(.8), width=.2) +
        geom_text(aes(y=!!sym(rate_col)+!!sym(se_col), label=.group),
                  position=position_dodge(.8), vjust=-.5, fontface="bold", size=4) +
        scale_fill_manual(values=pal) +
        theme_fn(base_size=fsize) +
        theme(plot.title=element_text(face="bold"), legend.position=leg_pos) +
        labs(y=paste("Taxa Estimada —", fam_label), title=paste("Interação Fatorial — GLM", fam_label))
    } else {
      em_fml <- as.formula(paste("~", rv$glm_trat))
      letras_df <- clean_cld(emmeans(m, em_fml, type=resp_type))
      
      rate_col <- if("rate" %in% names(letras_df)) "rate" else if("response" %in% names(letras_df)) "response" else "emmean"
      lcl_col <- if("asymp.LCL" %in% names(letras_df)) "asymp.LCL" else "lower.CL"
      ucl_col <- if("asymp.UCL" %in% names(letras_df)) "asymp.UCL" else "upper.CL"
      
      letras_df <- apply_plot_levels(letras_df, rv$glm_trat, input$custom_levels_1)
      
      p <- ggplot(letras_df, aes(x=reorder(!!sym(rv$glm_trat), -!!sym(rate_col)), y=!!sym(rate_col), fill=!!sym(rv$glm_trat))) +
        geom_col(color="black", width=.55, alpha=alpha_val) +
        geom_point(size=cur_pt_size()+1.5, color=pal[3]) +
        geom_errorbar(aes(ymin=!!sym(lcl_col), ymax=!!sym(ucl_col)), width=.15) +
        geom_text(aes(y=!!sym(ucl_col), label=.group), vjust=-.6, size=4.5, fontface="bold") +
        coord_flip() +
        scale_fill_manual(values=pal) +
        theme_fn(base_size=fsize) +
        theme(plot.title=element_text(face="bold"), legend.position="none") +
        labs(x="Tratamento", y=paste("Taxa Estimada —", fam_label), title=paste("Predições — GLM", fam_label))
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
  })
  
  output$plot_glm_dharma <- renderPlot({
    req(modelo_glm())
    m <- modelo_glm()
    if (is.list(m$family) && m$family$family == "quasipoisson") {
      m_pois <- glm(formula(m), family=poisson, data=m$data)
      plot(simulateResiduals(m_pois))
    } else {
      plot(simulateResiduals(m))
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
    
    items <- c(items, list(tags$div(class="interp-item", HTML("As letras no gr\u00e1fico indicam quais tratamentos s\u00e3o <b>estatisticamente diferentes</b> entre si pelo teste de Tukey. Tratamentos com a <b>mesma letra</b> n\u00e3o diferem significativamente (p>0.05)."))))
    
    tags$div(class="interp-box",
      h5(icon("lightbulb"), paste(" Interpreta\u00e7\u00e3o do GLM", fam_label)),
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
    }, error = function(e) { })
    
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
  
  output$dl_plot_reg     <- downloadHandler("regressao_plot.pdf", function(f) ggsave(f, export$plot_reg, device="pdf", width=cur_pdf_w(), height=cur_pdf_h()))
  output$dl_plot_reg_png <- downloadHandler("regressao_plot.png", function(f) ggsave(f, export$plot_reg, device="png", width=cur_pdf_w(), height=cur_pdf_h(), dpi=300))
  output$dl_plot_reg_svg <- downloadHandler("regressao_plot.svg", function(f) ggsave(f, export$plot_reg, device="svg", width=cur_pdf_w(), height=cur_pdf_h()))
  output$dl_res_reg  <- downloadHandler("regressao_res.txt",  function(f) writeLines(export$res_reg, f))
  
  # ══════════════════════════════════════════════════════════
  #  TAB 6 - AUDPC
  # ══════════════════════════════════════════════════════════
  audpc_summary <- reactive({
    req(rv$data, rv$audpc_time, rv$audpc_sev, rv$audpc_trat)
    rv$data |> group_by(!!sym(rv$audpc_trat), !!sym(rv$audpc_time)) |>
      summarise(mean_sev=mean(!!sym(rv$audpc_sev)*100, na.rm=TRUE), sd_sev=sd(!!sym(rv$audpc_sev)*100, na.rm=TRUE), .groups="drop")
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
      geom_errorbar(aes(ymin=mean_sev-sd_sev, ymax=mean_sev+sd_sev), width=.6, linewidth=.5) +
      scale_color_manual(values=pal) + scale_fill_manual(values=pal) +
      theme_fn(base_size=fsize) +
      theme(plot.title=element_text(face="bold"), plot.subtitle=element_text(color="gray40"), legend.position=leg_pos) +
      labs(x="Dias", y="Severidade Média (%)", color="Tratamento", fill="Tratamento",
           title="Curva de Progresso da Doença",
           subtitle="A área sombreada sob cada curva representa visualmente a AUDPC de cada tratamento")
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
        fit <- fit_lin(time=mean_df$time, y=mean_df$y, model=tolower(substr(epi_model_name, 1, 4)))
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
      scale_fill_manual(values=pal) +
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
}

shinyApp(ui, server)
