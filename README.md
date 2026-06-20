# FIP606 Studio

Este repositório contém o código-fonte de um aplicativo interativo (Shiny App) desenvolvido para a disciplina **FIP606 - Análise de Dados**. O aplicativo foca na facilidade e acessibilidade para realizar análises estatísticas, visualizações, modelagens experimentais e análises espaciais comumente utilizadas em agronomia e fitopatologia.

## 🚀 Funcionalidades

- **Importação de Dados**: Suporte para importação de planilhas de dados (Excel/CSV/TXT) e opção de conjuntos de dados de exemplo embutidos no app.
- **Visualização de Dados Interativa**: Geração de gráficos customizáveis utilizando `ggplot2` e `plotly`, com diversas paletas de cores, temas, e exportação em alta qualidade (PNG/SVG).
- **Múltiplos Módulos de Análise Estatística**: 
  - **Teste T**: Comparações simples de duas médias.
  - **ANOVA**: Análise de Variância incluindo delineamentos Fatorial, Blocos e Split-plot.
  - **Comparações Múltiplas e Testes de Médias**: Tukey, Scott-Knott, LSD (usando `emmeans`, `agricolae`).
  - **Modelos Lineares Generalizados (GLM)**: Ajuste para famílias Poisson, Binomial, Quasipoisson e Quasibinomial, com análise de qualidade via `DHARMa`.
  - **Regressão Linear e Polinomial**: Ajuste de modelos lineares e curvos (até grau 3).
  - **Correlação**: Matrizes de correlação (Pearson, Spearman) e gráficos de dispersão.
- **Módulos Epidemiológicos e Espaciais**:
  - **Curvas de Progresso da Doença & AACPD**: Cálculo da Área Abaixo da Curva de Progresso da Doença e modelagem epidemiológica.
  - **Detecção Espacial (Mapas)**: Plotagem de coordenadas geográficas, com detecção inteligente de polígonos usando `sf` e `rnaturalearth` (incluindo estados do Brasil).
- **Relatórios**: Geração automática de relatórios em Word/HTML usando `RMarkdown`.
- **Aulas Interativas**: Integração de recursos estáticos de ensino na própria aplicação.

## 📦 Dependências

O aplicativo foi desenvolvido em **R** e requer as seguintes bibliotecas para funcionar corretamente:

- `shiny`, `bslib`, `shinycssloaders`
- `tidyverse`, `readxl`, `DT`, `clipr`
- `ggplot2`, `plotly`, `patchwork`, `ggpubr`, `systemfonts`
- `rstatix`, `emmeans`, `multcompView`, `multcomp`, `lme4`, `lmerTest`, `agricolae`, `car`, `effectsize`
- `DHARMa`, `epifitter`
- `sf`, `rnaturalearth`, `rnaturalearthdata`

## ⚙️ Como executar localmente

1. Clone o repositório em sua máquina:
   ```bash
   git clone https://github.com/seu-usuario/FIP606_ShinyApp.git
   ```
2. Abra o projeto no **RStudio**.
3. Instale os pacotes requeridos listados acima. Você pode instalar as dependências com o seguinte comando no console do R:
   ```R
   install.packages(c("shiny", "bslib", "tidyverse", "ggplot2", "plotly", "readxl", "DT", "rstatix", "emmeans", "multcompView", "multcomp", "epifitter", "ggpubr", "DHARMa", "shinycssloaders", "lme4", "lmerTest", "agricolae", "car", "patchwork", "sf", "rnaturalearth", "rnaturalearthdata", "effectsize", "clipr", "systemfonts"))
   ```
4. Certifique-se de que o arquivo principal se chama `app.R`. Execute o aplicativo clicando em **Run App** no RStudio, ou executando:
   ```R
   shiny::runApp("app.R")
   ```

## 📖 Como usar o aplicativo

Após iniciar o aplicativo, a barra lateral o guiará pelas abas de análise:

1. **Início**: Tela de boas vindas com atalhos para os dados de exemplos embutidos no sistema.
2. **Dados e Transformações (AED)**: Importe os seus dados e defina as variáveis (Resposta, Tratamento 1, Tratamento 2, etc). Aqui também é possível realizar transformações logarítmicas ou box-cox.
3. **Análises Específicas**: Navegue pelas abas conforme o tipo do seu experimento:
   - *Teste T*, *ANOVA*, *GLM*, *Regressão*, *Área sob a Curva*, *Correlação* e *Mapas*.
4. **Gráficos e Resultados**: Cada aba possui seu próprio gerador de gráfico dinâmico, área de resultados estatísticos (com opção de copiar para o clipboard) e análises de pressupostos (ex: normalidade, homogenidade de variâncias).
5. **Geração de Relatório**: Utilize o botão flutuante de Relatório ou a própria opção no canto de cada análise para baixar os resultados (Word, HTML ou Imagem do gráfico).

## 🛠 Estrutura do Repositório

- `app.R`: Arquivo principal contendo a UI (Interface de Usuário) e a lógica de servidor (Server) do Shiny App.
- `report.Rmd`: Template em RMarkdown utilizado pelo aplicativo para gerar relatórios dinâmicos.
- `/aulas/`: Diretório contendo os recursos de aulas (arquivos HTML estáticos) servidos no app.
- `/data/`: Diretório destinado ao armazenamento de dados de exemplo (`.csv` e `.xlsx`).
- `/www/`: Imagens, logotipos e arquivos estáticos essenciais do sistema.

## 📝 Autores

Desenvolvido por **Milton E. C. M. Galvino** e **Laura G. Agudelo** para a disciplina FIP606 - Análise de Dados.
