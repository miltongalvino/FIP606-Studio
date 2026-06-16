# FIP606 Studio

Este repositório contém o código-fonte de um aplicativo interativo (Shiny App) desenvolvido para a disciplina **FIP606 - Análise de Dados**. O aplicativo foca na facilidade e acessibilidade para realizar análises estatísticas, visualizações e modelagens experimentais comumente utilizadas.

## 🚀 Funcionalidades

- **Importação de Dados**: Suporte para importação de planilhas de dados (Excel/CSV/TXT).
- **Visualização de Dados Interativa**: Geração de gráficos customizáveis utilizando `ggplot2` e `plotly`, com diversas paletas de cores, temas e controle de design.
- **Análise Exploratória e Transformação**: Opções de transformações de variáveis (Log, Raiz Quadrada, Box-Cox, etc.).
- **Análise Estatística**: 
  - Análise de Variância (ANOVA).
  - Comparações Múltiplas e Testes de Médias (usando `emmeans`, `multcomp`, `rstatix`).
  - Análise de Resíduos e Qualidade de Ajuste de Modelos (usando `DHARMa`).
  - Testes Não Paramétricos.
- **Relatórios**: Geração de relatórios com `RMarkdown`.
- **Aulas Interativas**: Integração de recursos estáticos de ensino na própria aplicação.

## 📦 Dependências

O aplicativo foi desenvolvido em **R** e requer as seguintes bibliotecas para funcionar corretamente:

- `shiny`
- `bslib`
- `tidyverse`
- `ggplot2`
- `plotly`
- `readxl`
- `DT`
- `rstatix`
- `emmeans`
- `multcompView`
- `multcomp`
- `epifitter`
- `ggpubr`
- `DHARMa`
- `shinycssloaders`

## ⚙️ Como executar localmente

1. Clone o repositório em sua máquina:
   ```bash
   git clone https://github.com/seu-usuario/FIP606_ShinyApp.git
   ```
2. Abra o projeto no **RStudio**.
3. Instale os pacotes requeridos listados acima. Você pode instalar as dependências com o seguinte comando no console do R:
   ```R
   install.packages(c("shiny", "bslib", "tidyverse", "ggplot2", "plotly", "readxl", "DT", "rstatix", "emmeans", "multcompView", "multcomp", "epifitter", "ggpubr", "DHARMa", "shinycssloaders"))
   ```
4. Execute o aplicativo abrindo o arquivo `app_v6.R` e clicando em **Run App** no RStudio, ou executando:
   ```R
   shiny::runApp("app_v6.R")
   ```

## 📖 Como usar o aplicativo

Após iniciar o aplicativo, siga as etapas abaixo para realizar sua análise:

1. **Importação e Configuração**:
   - Vá até a aba **"Dados"**.
   - Faça o upload do seu arquivo de dados (suporta formatos Excel, CSV e TXT).
   - Especifique qual coluna representa o **Tratamento** (variável independente/fator) e qual representa a **Resposta** (variável dependente).
   - *(Opcional)* Se você possuir blocos ou outro fator no delineamento, selecione a coluna correspondente em "Fator 2 / Bloco".

2. **Visualização de Dados**:
   - Na aba **"Gráficos"**, você poderá visualizar o comportamento dos seus dados através de Boxplots, Gráficos de Barras, Pontos, etc.
   - Use o painel lateral para personalizar completamente a aparência do gráfico (títulos, cores, eixos, temas e paletas de cores prontas).

3. **Análise de Variância (ANOVA) e Pressupostos**:
   - Acesse a aba **"Análise"**.
   - O aplicativo fará o ajuste do modelo e testará os pressupostos da ANOVA (Normalidade dos resíduos e Homogeneidade de Variâncias).
   - Caso os pressupostos não sejam atendidos, você pode retornar à aba de "Gráficos" ou "Dados" para aplicar uma **Transformação** (ex: Box-Cox, Log) ou optar por utilizar os **Testes Não-Paramétricos** oferecidos pelo app.
   - Em seguida, visualize a Tabela da ANOVA.

4. **Teste de Médias (Comparações Múltiplas)**:
   - Se a ANOVA for significativa, você poderá visualizar o teste de médias comparando os tratamentos (ex: Teste de Tukey, Scott-Knott, etc.).
   - As letras indicativas de diferenças significativas podem ser adicionadas automaticamente aos gráficos.

5. **Geração de Relatório**:
   - Após concluir suas análises, você pode gerar e baixar um relatório completo contendo os resultados numéricos, as conclusões estatísticas e os gráficos gerados clicando no botão de exportar/relatório.

## 🛠 Estrutura do Repositório

- `app_v6.R`: Arquivo principal contendo a UI (Interface de Usuário) e a lógica de servidor (Server) do Shiny App.
- `report.Rmd`: Template em RMarkdown utilizado pelo aplicativo para gerar relatórios dinâmicos da análise estatística do usuário.
- `/aulas/`: Diretório contendo os recursos de aulas (arquivos HTML estáticos) servidos no app.
- `/data/`: Diretório destinado ao armazenamento de dados de exemplo.
- `/rsconnect/`: Metadados gerados em caso de deploy do app em plataformas como o shinyapps.io.

## 📝 Autores

Desenvolvido por Milton E. C. M. Galvino e Laura G. Agudelo.

