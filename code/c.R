# ==============================================================================
# 專案名稱:NBA 球員球風 K-Means 分群動態觀測站 (Scouting Tool 版)
# 功能:全賽季整合 + 交叉矩陣評估 + 一鍵匯出 CSV + 球員深度分析
#
# 階段一(視覺):
#   1. navbar 分頁式架構,把九張卡片拆成多個主題分頁,不再擠成一團
#   2. 頂部 value boxes 即時顯示關鍵指標
#   3. 全面改用 DT 互動表格(可搜尋、排序、分頁)
#   4. 新增雷達圖、交叉矩陣熱力圖
#   5. 自訂 CSS:卡片陰影、圓角、字體、配色
#   6. 分群顏色語意化:每群用固定色票貫穿全站
#
# 階段二(球員層級分析 — NEW):
#   1. 球員搜尋(selectizeInput,即時搜尋)
#   2. 五維雷達:scoring / playmaking / rebounding / defense / shooting
#      三條曲線同時顯示:球員本人、同群平均、全體平均(以百分位呈現)
#   3. 各項數據對照表:球員值 vs 同群平均 vs 全體平均 + 差距 %
#   4. 群內排名:得分、助攻、籃板、抄截、阻攻、防守(STL+BLK)
#   5. 相似球員推薦:基於 9 項技術指標標準化後的歐氏距離,Top 10
# ==============================================================================

library(shiny)
library(bslib)
library(bsicons)        # ⚠️ 如未安裝請執行 install.packages("bsicons")
library(tidyverse)
library(cluster)
library(plotly)
library(DT)             # ⚠️ 如未安裝請執行 install.packages("DT")

# ==========================================
# 0. 讀檔
# ==========================================
csv_file <- "data/nba_clustered_players.csv"

if (!file.exists(csv_file)) {
  stop("錯誤:在目前目錄下找不到 nba_clustered_players.csv 檔案!")
}

RAW_NBA_DATA <- read_csv(csv_file, show_col_types = FALSE) %>%
  mutate(
    position_raw = trimws(as.character(position)),
    position = substr(position_raw, 1, 1),
    Season = trimws(as.character(Season))
  )

FEATURES <- c('points', 'assists', 'reboundsOffensive',
              'reboundsDefensive', 'steals', 'blocks', 'FG%', '3P%', 'FT%')

AVAILABLE_SEASONS <- c("所有賽季 (16-26)", unique(RAW_NBA_DATA$Season) %>% sort())
INITIAL_PLAYER_CHOICES <- RAW_NBA_DATA %>%
  filter(gameId >= 30, numMinutes >= 15) %>%
  arrange(name, Season) %>%
  mutate(player_display = paste0(name, " | ", Season)) %>%
  pull(player_display) %>%
  unique()
INITIAL_PLAYER_CHOICES_PREVIEW <- head(INITIAL_PLAYER_CHOICES, 50)

# 分群顏色語意化(全站一致)
CLUSTER_COLORS <- c(
  "禁區守護神 (Rim Protector)"   = "#E74C3C",
  "全能組織核心 (Playmaker)"     = "#3498DB",
  "高效得分暴徒 (Scoring Spark)" = "#F39C12",
  "外線冷血射手 (3-and-D)"       = "#1ABC9C"
)

# ==========================================
# 自訂 CSS — 整體質感升級
# ==========================================
custom_css <- "
body {
  background: #f4f6f9;
  font-family: 'Noto Sans TC', -apple-system, BlinkMacSystemFont, sans-serif;
}
.navbar {
  box-shadow: 0 2px 12px rgba(0,0,0,0.08);
}
.bslib-value-box {
  border: none !important;
  box-shadow: 0 2px 12px rgba(0,0,0,0.06);
  border-radius: 14px;
  transition: transform 0.2s ease, box-shadow 0.2s ease;
}
.bslib-value-box:hover {
  transform: translateY(-2px);
  box-shadow: 0 6px 20px rgba(0,0,0,0.1);
}
.bslib-value-box .value-box-title {
  font-size: 0.78rem;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 0.06em;
  opacity: 0.92;
}
.bslib-value-box .value-box-value {
  font-size: 2.1rem;
  font-weight: 700;
  letter-spacing: -0.02em;
}
.card {
  border: none !important;
  box-shadow: 0 2px 10px rgba(0,0,0,0.05);
  border-radius: 14px;
  overflow: visible;
  background: white;
}
.player-selector-card {
  position: relative;
  z-index: 30;
}
.selectize-dropdown {
  z-index: 4000 !important;
}
.player-analysis-accordion {
  margin-top: 1rem;
}
.player-analysis-accordion .accordion-item {
  border: none;
  border-radius: 14px;
  box-shadow: 0 2px 10px rgba(0,0,0,0.05);
  margin-bottom: 1rem;
  overflow: hidden;
}
.player-analysis-accordion .accordion-button {
  font-weight: 600;
  color: #2c3e50;
  background: white;
}
.player-analysis-accordion .accordion-button:not(.collapsed) {
  background: #f0fbf8;
  color: #128f78;
}
.card-header {
  background: white !important;
  border-bottom: 1px solid #eef1f5 !important;
  font-weight: 600;
  font-size: 0.98rem;
  color: #2c3e50;
  padding: 1rem 1.25rem;
}
.card-header .bi {
  color: #18bc9c;
  margin-right: 6px;
}
.card-footer {
  background: #fafbfc !important;
  font-size: 0.8rem;
  color: #7f8c8d;
  border-top: 1px solid #eef1f5 !important;
  padding: 0.7rem 1.25rem;
}
.bslib-sidebar-layout > .sidebar {
  background: white !important;
  border-right: 1px solid #eef1f5;
}
.sidebar .form-label, .sidebar label {
  font-weight: 600;
  color: #2c3e50;
  font-size: 0.88rem;
}
.form-control, .form-select {
  border-radius: 8px;
  border-color: #dde3ea;
}
.irs--shiny .irs-bar, .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
  background: #18bc9c;
}
.irs--shiny .irs-handle {
  border-color: #18bc9c;
}
.btn-success {
  background: linear-gradient(135deg, #18bc9c, #15a589);
  border: none;
  border-radius: 10px;
  font-weight: 600;
  padding: 0.7rem 1rem;
  letter-spacing: 0.02em;
  box-shadow: 0 2px 8px rgba(24, 188, 156, 0.3);
}
.btn-success:hover {
  background: linear-gradient(135deg, #15a589, #128f78);
  transform: translateY(-1px);
  box-shadow: 0 4px 12px rgba(24, 188, 156, 0.4);
}
.navbar-nav .nav-link {
  color: rgba(255,255,255,0.78) !important;
  font-weight: 500;
  padding: 0.42rem 0.9rem !important;
  border-radius: 8px;
  transition: background 0.18s ease, color 0.18s ease;
  white-space: nowrap;
}
.navbar-nav .nav-link:hover {
  color: #ffffff !important;
  background-color: rgba(255,255,255,0.12) !important;
}
.navbar-nav .nav-link.active {
  color: #ffffff !important;
  background-color: rgba(255,255,255,0.20) !important;
  font-weight: 600;
}
.navbar-nav .nav-link .bi,
.navbar-nav .nav-link svg {
  color: inherit !important;
  fill: currentColor !important;
}
.nav-tabs .nav-link {
  font-weight: 500;
}
table.dataTable thead th {
  background: #2c3e50 !important;
  color: white !important;
  font-weight: 600 !important;
  border-bottom: none !important;
}
table.dataTable tbody tr:hover {
  background-color: #f0fbf8 !important;
}
.dataTables_wrapper .dataTables_filter input {
  border-radius: 8px;
  border: 1px solid #dde3ea;
  padding: 5px 10px;
}
hr {
  border-color: #eef1f5;
  margin: 1.25rem 0;
}
"

# ==========================================
# 1. UI
# ==========================================
ui <- page_navbar(
  title = tags$span(
    bs_icon("graph-up-arrow"), " NBA 球風分群觀測站"
  ),
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2c3e50",
    success = "#18bc9c",
    base_font = font_google("Noto Sans TC"),
    heading_font = font_google("Noto Sans TC")
  ),
  header = tags$head(tags$style(HTML(custom_css))),
  navbar_options = navbar_options(bg = "#2c3e50"),
  
  sidebar = sidebar(
    width = 290,
    title = tags$span(bs_icon("sliders"), " 篩選條件"),
    
    selectInput("season", "📅 觀測賽季",
                choices = AVAILABLE_SEASONS,
                selected = AVAILABLE_SEASONS[1]),
    
    sliderInput("min_games", "🎯 最低上場場次",
                min = 0, max = 82, value = 30, step = 1),
    
    sliderInput("min_mins", "⏱️ 最低平均上場分鐘",
                min = 0, max = 48, value = 15, step = 1),
    
    hr(),
    
    downloadButton("download_csv", " 匯出分群名單 CSV",
                   class = "btn-success w-100",
                   icon = icon("download")),
    
    hr(),
    
    div(
      style = "font-size: 0.8rem; color: #7f8c8d; line-height: 1.55;",
      bs_icon("info-circle"),
      " 調整參數後,K-Means 模型將即時重跑並更新全站圖表。"
    )
  ),
  
  # ============ 分頁 1:總覽 ============
  nav_panel(
    title = tags$span(bs_icon("speedometer2"), " 總覽"),

    div(
      style = "display: flex; flex-direction: column; gap: 2rem; padding: 0.5rem 0 2rem;",

      # Row 1:四格 Value Box
      layout_columns(
        fill = FALSE,
        col_widths = c(4, 4, 4),
        value_box(
          title = "符合條件球員數",
          value = textOutput("total_players"),
          showcase = bs_icon("people-fill"),
          theme = "primary"
        ),
        value_box(
          title = "K=4 Silhouette 分數",
          value = textOutput("k4_score"),
          showcase = bs_icon("bullseye"),
          theme = "info"
        ),
        value_box(
          title = "觀測賽季",
          value = textOutput("current_season"),
          showcase = bs_icon("calendar-week"),
          theme = "warning"
        )
      ),

      # Row 2:分群人數分布 + Silhouette 曲線
      layout_columns(
        col_widths = c(5, 7),
        gap = "2rem",
        card(
          card_header(bs_icon("people-fill"), " K=4 各分群球員人數"),
          plotlyOutput("cluster_dist_plot", height = "320px"),
          card_footer("當前篩選條件下，四個球風分群各自涵蓋的球員數量。")
        ),
        card(
          card_header(bs_icon("graph-up"), " Silhouette Score 曲線 (最佳 K 搜尋)"),
          plotlyOutput("sil_plot", height = "320px"),
          card_footer("紅虛線為本應用採用之 K=4。")
        )
      ),

      # Row 3:K 值品質表格
      card(
        card_header(bs_icon("table"), " K 值品質比較"),
        div(style = "padding: 0.75rem 0.25rem;",
            DTOutput("k_quality_table")),
        card_footer("平衡度越接近 1，各群人數越平均；位置集中度越高，分群越接近傳統 C/F/G。")
      )
    )
  ),
  
  # ============ 分頁 2:球員深度分析 (Scouting Tool) ============
  nav_panel(
    title = tags$span(bs_icon("person-bounding-box"), " 球員分析"),
    
    # ▶ Row 1:球員搜尋
    div(
      style = paste0(
        "background: white; border-radius: 14px; ",
        "box-shadow: 0 2px 12px rgba(0,0,0,0.07); ",
        "padding: 1.1rem 1.4rem 0.9rem; margin-bottom: 1rem; ",
        "position: relative; z-index: 30; overflow: visible;"
      ),
      div(
        class = "d-flex align-items-start gap-3",
        div(
          style = "padding-top: 0.3rem; color: #18bc9c; flex-shrink: 0;",
          bs_icon("person-lines-fill", size = "1.8em")
        ),
        div(
          style = "flex: 1; min-width: 0;",
          div(
            style = "font-weight: 700; font-size: 1rem; color: #2c3e50; margin-bottom: 0.45rem;",
            "選擇球員進行深度分析"
          ),
          selectizeInput(
            "selected_player",
            label = NULL,
            choices = INITIAL_PLAYER_CHOICES_PREVIEW,
            selected = if (length(INITIAL_PLAYER_CHOICES) > 0) INITIAL_PLAYER_CHOICES[1] else character(0),
            width = "100%",
            options = list(
              placeholder = "輸入球員姓名搜尋... (例:LeBron James、Stephen Curry)",
              maxOptions = 50,
              openOnFocus = TRUE
            )
          ),
          div(
            style = "font-size: 0.78rem; color: #95a5a6; margin-top: 0.2rem;",
            bs_icon("arrow-left-right"), " 篩選條件與左側篩選器連動，變更後選單自動同步。"
          )
        )
      )
    ),
    
    # ▶ Row 2:球員身份卡片
    layout_columns(
      fill = FALSE,
      col_widths = c(3, 3, 3, 3),
      value_box(
        title = "所屬分群",
        value = textOutput("player_cluster"),
        showcase = bs_icon("tags-fill"),
        theme = "primary"
      ),
      value_box(
        title = "賽季",
        value = textOutput("player_season"),
        showcase = bs_icon("calendar-week"),
        theme = "secondary"
      ),
      value_box(
        title = "註冊位置",
        value = textOutput("player_position"),
        showcase = bs_icon("geo-alt-fill"),
        theme = "info"
      ),
      value_box(
        title = "出場 / 平均分鐘",
        value = textOutput("player_games_mins"),
        showcase = bs_icon("clock-fill"),
        theme = "warning"
      )
    ),
    
    accordion(
      id = "player_analysis_sections",
      multiple = TRUE,
      open = c("五維雷達", "各項數據:球員 vs 平均"),
      class = "player-analysis-accordion",
      accordion_panel(
        "五維雷達",
        plotlyOutput("player_radar", height = "520px"),
        div(class = "card-footer", "數值為篩選樣本中的百分位 (0–100)。實心:球員本人;虛線:同群平均;灰點線:全體平均(理論值 50)。")
      ),
      accordion_panel(
        "各項數據:球員 vs 平均",
        DTOutput("player_comparison"),
        div(class = "card-footer", "「vs 同群」與「vs 全體」為相對差距(%);正值代表球員高於該基準。")
      ),
      accordion_panel(
        "群內排名",
        DTOutput("player_ranks"),
        div(class = "card-footer", "百分位 90% 代表勝過該群中 90% 的球員。")
      ),
      accordion_panel(
        "風格最相似的球員 (Top 10)",
        DTOutput("similar_players_table"),
        div(class = "card-footer", "基於 9 項技術指標標準化後的歐氏距離,距離越小越相似。相似球員可能跨分群。")
      )
    )
  ),
  
  # ============ 分頁 3:分群輪廓 ============
  nav_panel(
    title = tags$span(bs_icon("diagram-3"), " 分群輪廓"),

    div(
      style = "display: flex; flex-direction: column; gap: 2rem; padding: 0.5rem 0 2rem;",

      # Row 1:四群快覽摘要
      card(
        card_header(bs_icon("collection-fill"), " K=4 分群摘要"),
        div(style = "padding: 0.75rem 0.25rem;",
            DTOutput("cluster_summary_table")),
        card_footer("每群的規模、平均出場與核心 box-score 輪廓。")
      ),

      # Row 2:特色標籤 + 雷達圖並排
      layout_columns(
        col_widths = c(5, 7),
        gap = "2rem",
        card(
          card_header(bs_icon("star-fill"), " 各群最鮮明技術特色"),
          div(style = "padding: 0.75rem 0.25rem;",
              DTOutput("cluster_signature_table")),
          card_footer("每群相對全體平均高出最多的三項指標。")
        ),
        card(
          card_header(bs_icon("bullseye"), " 雷達圖:群組技術剖面"),
          plotlyOutput("radar_plot", height = "500px"),
          card_footer("各群於每項指標相對全體平均的倍率 (1.0 = 平均水準)。")
        )
      ),

      # Row 3:兩張數字表格用 tab 合併
      navset_card_tab(
        title = tagList(bs_icon("table"), " 詳細數據"),
        nav_panel(
          tagList(bs_icon("123"), " 各群技術特徵平均值"),
          div(style = "padding: 1rem 0.25rem 0.5rem;",
              DTOutput("cluster_means_table"))
        ),
        nav_panel(
          tagList(bs_icon("arrow-up-right-circle"), " Lift % 差距"),
          div(style = "padding: 1rem 0.25rem 0;",
              DTOutput("cluster_lift_table")),
          div(class = "card-footer", "正值代表該群在此指標高於全體平均。")
        )
      )
    )
  ),
  
  # ============ 分頁 4:位置對比 ============
  nav_panel(
    title = tags$span(bs_icon("intersect"), " 位置對比"),

    div(
      style = "display: flex; flex-direction: column; gap: 2rem; padding: 0.5rem 0 2rem;",

      # Row 1:各分群的傳統位置組成(堆疊長條)
      card(
        card_header(bs_icon("bar-chart-steps"), " 各分群的傳統位置組成"),
        plotlyOutput("position_bar", height = "380px"),
        card_footer("每個球風分群中，C / F / G 各占多少比例。顯示數據分群如何打破傳統位置邊界。")
      ),

      # Row 2:熱力圖 + 交叉表
      layout_columns(
        col_widths = c(7, 5),
        gap = "2rem",
        card(
          card_header(bs_icon("grid-3x3-gap-fill"), " 傳統位置 × 數據分群 熱力圖"),
          plotlyOutput("cross_heatmap", height = "440px"),
          card_footer("色塊越深代表人數越多。一眼看出傳統 C/F/G 在新分群中如何重組。")
        ),
        card(
          card_header(bs_icon("table"), " 交叉矩陣明細"),
          div(style = "padding: 0.75rem 0.25rem;",
              DTOutput("cross_tab")),
          card_footer("傳統註冊位置 (C/F/G) 在數據分群下的分布人數。")
        )
      )
    )
  ),
  
  # ============ 分頁 5:球員名單 ============
  nav_panel(
    title = tags$span(bs_icon("person-lines-fill"), " 球員名單"),
    
    card(
      card_header(bs_icon("list-ul"), " 當前條件下之球員分群名單明細"),
      DTOutput("player_detail_table"),
      card_footer("可使用右上方搜尋框輸入球員姓名,或點欄位標題排序。")
    )
  )
)

# ==========================================
# 2. Server
# ==========================================
server <- function(input, output, session) {
  
  # 響應式 1:過濾資料
  filtered_data <- reactive({
    if (input$season == "所有賽季 (16-26)") {
      RAW_NBA_DATA %>%
        filter(gameId >= input$min_games, numMinutes >= input$min_mins)
    } else {
      RAW_NBA_DATA %>%
        filter(
          Season == trimws(as.character(input$season)),
          gameId >= input$min_games,
          numMinutes >= input$min_mins
        )
    }
  })
  
  # 響應式 2:模型計算核心
  model_pipeline <- reactive({
    df_sub <- filtered_data()
    if (nrow(df_sub) < 15) return(NULL)
    
    X <- df_sub[, FEATURES] %>% mutate(across(everything(), ~replace_na(., 0)))
    X_scaled <- scale(X)
    distance_matrix <- dist(X_scaled)
    
    k_range <- 2:8
    k_quality_raw <- map_dfr(k_range, function(k) {
      set.seed(42)
      km <- kmeans(X_scaled, centers = k, nstart = 10)
      sil <- silhouette(km$cluster, distance_matrix)
      cluster_sizes <- as.integer(table(km$cluster))
      position_table <- table(km$cluster, df_sub$position)
      position_alignment <- sum(apply(position_table, 1, max)) / nrow(df_sub)
      
      tibble(
        K = k,
        sil_score = mean(sil[, 3]),
        min_cluster_size = min(cluster_sizes),
        max_cluster_size = max(cluster_sizes),
        balance_ratio = max(cluster_sizes) / min(cluster_sizes),
        largest_cluster_share = max(cluster_sizes) / nrow(df_sub),
        position_alignment = position_alignment
      )
    })
    sil_scores <- k_quality_raw$sil_score
    
    k_quality_table <- k_quality_raw %>%
      transmute(
        K,
        `Silhouette` = round(sil_score, 3),
        `最小群` = min_cluster_size,
        `最大群` = max_cluster_size,
        `平衡度` = round(balance_ratio, 2),
        `最大群占比` = paste0(round(largest_cluster_share * 100, 1), "%"),
        `位置集中度` = paste0(round(position_alignment * 100, 1), "%")
      )
    
    set.seed(42)
    final_km <- kmeans(X_scaled, centers = 4, nstart = 10)
    
    means_temp <- df_sub %>%
      mutate(Cluster_Num = final_km$cluster) %>%
      group_by(Cluster_Num) %>%
      summarise(
        m_pts = mean(points, na.rm = TRUE),
        m_ast = mean(assists, na.rm = TRUE),
        m_3p  = mean(`3P%`, na.rm = TRUE),
        .groups = 'drop'
      )
    
    c_center <- means_temp$Cluster_Num[which.min(means_temp$m_3p)]
    remaining <- setdiff(1:4, c_center)
    means_rem <- means_temp %>% filter(Cluster_Num %in% remaining)
    c_guard <- means_rem$Cluster_Num[which.max(means_rem$m_ast)]
    remaining <- setdiff(remaining, c_guard)
    means_rem2 <- means_temp %>% filter(Cluster_Num %in% remaining)
    c_scorer <- means_rem2$Cluster_Num[which.max(means_rem2$m_pts)]
    c_3d <- setdiff(remaining, c_scorer)
    
    df_with_cluster <- df_sub %>%
      mutate(
        Cluster_Num = final_km$cluster,
        Cluster = case_when(
          Cluster_Num == c_center ~ "禁區守護神 (Rim Protector)",
          Cluster_Num == c_guard  ~ "全能組織核心 (Playmaker)",
          Cluster_Num == c_scorer ~ "高效得分暴徒 (Scoring Spark)",
          Cluster_Num == c_3d     ~ "外線冷血射手 (3-and-D)",
          TRUE ~ paste("Cluster", Cluster_Num)
        ),
        player_display = paste0(name, " | ", Season),
        row_idx = row_number()
      )
    
    cluster_means <- df_with_cluster %>%
      group_by(Cluster) %>%
      summarise(across(all_of(FEATURES), ~round(mean(., na.rm = TRUE), 2)), .groups = 'drop')
    
    cluster_summary <- df_with_cluster %>%
      group_by(Cluster) %>%
      summarise(
        `球員數` = n(),
        `平均場次` = round(mean(gameId, na.rm = TRUE), 1),
        `平均分鐘` = round(mean(numMinutes, na.rm = TRUE), 1),
        PTS = round(mean(points, na.rm = TRUE), 1),
        AST = round(mean(assists, na.rm = TRUE), 1),
        REB = round(mean(reboundsTotal, na.rm = TRUE), 1),
        STL = round(mean(steals, na.rm = TRUE), 1),
        BLK = round(mean(blocks, na.rm = TRUE), 1),
        `3P%` = round(mean(`3P%`, na.rm = TRUE), 3),
        .groups = 'drop'
      ) %>%
      arrange(desc(`球員數`))
    
    overall_means <- df_with_cluster %>%
      summarise(across(all_of(FEATURES), ~mean(., na.rm = TRUE))) %>%
      pivot_longer(everything(), names_to = "metric", values_to = "overall_mean")
    
    cluster_lift_raw <- df_with_cluster %>%
      group_by(Cluster) %>%
      summarise(across(all_of(FEATURES), ~mean(., na.rm = TRUE)), .groups = 'drop') %>%
      pivot_longer(-Cluster, names_to = "metric", values_to = "cluster_mean") %>%
      left_join(overall_means, by = "metric") %>%
      mutate(
        lift_pct = if_else(
          is.na(overall_mean) | overall_mean == 0,
          NA_real_,
          (cluster_mean / overall_mean - 1) * 100
        )
      )
    
    cluster_lift_table <- cluster_lift_raw %>%
      transmute(
        Cluster,
        `指標` = metric,
        `群組平均` = round(cluster_mean, 3),
        `全體平均` = round(overall_mean, 3),
        `差距` = if_else(
          is.na(lift_pct),
          "N/A",
          paste0(if_else(lift_pct >= 0, "+", ""), round(lift_pct, 1), "%")
        )
      ) %>%
      arrange(Cluster, `指標`)
    
    cluster_signature_table <- cluster_lift_raw %>%
      filter(!is.na(lift_pct)) %>%
      group_by(Cluster) %>%
      slice_max(lift_pct, n = 3, with_ties = FALSE) %>%
      arrange(Cluster, desc(lift_pct)) %>%
      summarise(
        `高於平均最多的三項指標` = paste0(metric, " +", round(lift_pct, 1), "%", collapse = "  ▪  "),
        .groups = 'drop'
      )
    
    expanded_pos <- df_with_cluster %>%
      mutate(all_pos = strsplit(position_raw, " ")) %>%
      tidyr::unnest(all_pos) %>%
      mutate(all_pos = substr(trimws(all_pos), 1, 1)) %>%
      filter(nchar(all_pos) > 0)

    cross_data <- table(expanded_pos$all_pos, expanded_pos$Cluster) %>%
      as.data.frame.matrix() %>%
      rownames_to_column(var = "傳統位置")

    cross_long <- expanded_pos %>%
      count(position = all_pos, Cluster) %>%
      complete(position, Cluster, fill = list(n = 0))
    
    list(
      k_range = k_range,
      sil_scores = sil_scores,
      k_quality_table = k_quality_table,
      k_quality_raw = k_quality_raw,
      df_result = df_with_cluster,
      cluster_summary = cluster_summary,
      cluster_signature = cluster_signature_table,
      cluster_means = cluster_means,
      cluster_lift = cluster_lift_table,
      cluster_lift_raw = cluster_lift_raw,
      cross_table = cross_data,
      cross_long = cross_long,
      X_scaled = X_scaled
    )
  })
  
  # ====== Value Boxes ======
  output$total_players <- renderText({
    mp <- model_pipeline()
    if (is.null(mp)) return("—")
    format(nrow(mp$df_result), big.mark = ",")
  })
  
  output$best_k <- renderText({
    mp <- model_pipeline()
    if (is.null(mp)) return("—")
    best <- mp$k_quality_raw$K[which.max(mp$k_quality_raw$sil_score)]
    paste0("K = ", best)
  })
  
  output$k4_score <- renderText({
    mp <- model_pipeline()
    if (is.null(mp)) return("—")
    score <- mp$k_quality_raw$sil_score[mp$k_quality_raw$K == 4]
    sprintf("%.3f", score)
  })
  
  output$current_season <- renderText({
    input$season
  })
  
  # ====== Silhouette Plot (plotly) ======
  output$sil_plot <- renderPlotly({
    mp <- model_pipeline()
    if (is.null(mp)) return(NULL)
    
    df <- tibble(K = mp$k_range, Score = mp$sil_scores)
    
    plot_ly(df, x = ~K, y = ~Score, type = "scatter", mode = "lines+markers",
            line = list(color = "#2c3e50", width = 3, shape = "spline"),
            marker = list(color = "#18bc9c", size = 11,
                          line = list(color = "white", width = 2)),
            hovertemplate = "K = %{x}<br>Silhouette = %{y:.3f}<extra></extra>") %>%
      add_segments(x = 4, xend = 4,
                   y = min(df$Score) - 0.005, yend = max(df$Score) + 0.005,
                   line = list(color = "#e74c3c", dash = "dash", width = 2),
                   showlegend = FALSE, hoverinfo = "skip") %>%
      layout(
        xaxis = list(title = "群集數量 (K)", gridcolor = "#ecf0f1", dtick = 1, zeroline = FALSE),
        yaxis = list(title = "Silhouette Score", gridcolor = "#ecf0f1", zeroline = FALSE),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)",
        font = list(family = "Noto Sans TC, sans-serif", size = 12, color = "#2c3e50"),
        margin = list(l = 50, r = 20, t = 20, b = 50),
        showlegend = FALSE
      ) %>% config(displayModeBar = FALSE)
  })
  
  # ====== Cluster Distribution Bar ======
  output$cluster_dist_plot <- renderPlotly({
    mp <- model_pipeline()
    if (is.null(mp)) return(NULL)

    df <- mp$cluster_summary %>%
      mutate(
        short_name = gsub(" \\(.*\\)", "", Cluster),
        bar_color  = CLUSTER_COLORS[as.character(Cluster)]
      ) %>%
      arrange(`球員數`)   # 由少到多，橫向長條由下往上遞增

    plot_ly(
      df,
      y = ~short_name,
      x = ~`球員數`,
      type = "bar",
      orientation = "h",
      marker = list(
        color = df$bar_color,
        line  = list(color = "white", width = 0)
      ),
      text  = ~paste0(`球員數`, " 人"),
      textposition = "outside",
      cliponaxis = FALSE,
      hovertemplate = "<b>%{y}</b><br>球員數: %{x}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(
          title = "球員數", gridcolor = "#ecf0f1",
          zeroline = FALSE, range = c(0, max(df$`球員數`) * 1.18)
        ),
        yaxis = list(title = "", gridcolor = "#ecf0f1", zeroline = FALSE),
        plot_bgcolor  = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)",
        font   = list(family = "Noto Sans TC, sans-serif", size = 12, color = "#2c3e50"),
        margin = list(l = 10, r = 70, t = 15, b = 45),
        showlegend = FALSE
      ) %>%
      config(displayModeBar = FALSE)
  })

  # ====== Radar Plot ======
  output$radar_plot <- renderPlotly({
    mp <- model_pipeline()
    if (is.null(mp)) return(NULL)
    
    radar_df <- mp$cluster_lift_raw %>%
      mutate(
        ratio = if_else(overall_mean == 0 | is.na(overall_mean), 1, cluster_mean / overall_mean),
        ratio = pmin(pmax(ratio, 0), 2.5)  # 裁切防極端值
      )
    
    fig <- plot_ly(type = "scatterpolar", mode = "lines+markers")
    
    clusters <- unique(radar_df$Cluster)
    for (cl in clusters) {
      sub <- radar_df %>% filter(Cluster == cl)
      color <- CLUSTER_COLORS[cl]
      if (is.na(color)) color <- "#7f8c8d"
      
      fig <- fig %>% add_trace(
        r = c(sub$ratio, sub$ratio[1]),
        theta = c(sub$metric, sub$metric[1]),
        name = cl,
        line = list(color = color, width = 2),
        marker = list(color = color, size = 6),
        fill = "toself",
        fillcolor = paste0(color, "25")
      )
    }
    
    fig %>% layout(
      polar = list(
        radialaxis = list(visible = TRUE, gridcolor = "#ecf0f1", range = c(0, 2.5)),
        angularaxis = list(gridcolor = "#ecf0f1")
      ),
      showlegend = TRUE,
      legend = list(orientation = "h", y = -0.15, font = list(size = 10)),
      font = list(family = "Noto Sans TC, sans-serif", size = 11, color = "#2c3e50"),
      paper_bgcolor = "rgba(0,0,0,0)",
      margin = list(l = 40, r = 40, t = 30, b = 50)
    ) %>% config(displayModeBar = FALSE)
  })
  
  # ====== Cross-tab Heatmap ======
  output$cross_heatmap <- renderPlotly({
    mp <- model_pipeline()
    if (is.null(mp)) return(NULL)
    
    cl <- mp$cross_long
    
    plot_ly(
      cl, x = ~Cluster, y = ~position, z = ~n,
      type = "heatmap",
      colorscale = list(c(0, "#f4f6f9"), c(0.4, "#5dade2"), c(1, "#1a2e44")),
      hovertemplate = "傳統位置: %{y}<br>分群: %{x}<br>人數: %{z}<extra></extra>",
      colorbar = list(title = list(text = "人數", font = list(size = 11)), thickness = 12)
    ) %>%
      add_annotations(
        x = cl$Cluster, y = cl$position, text = cl$n,
        showarrow = FALSE,
        font = list(color = ifelse(cl$n > max(cl$n) * 0.4, "white", "#2c3e50"),
                    size = 14, family = "Noto Sans TC")
      ) %>%
      layout(
        xaxis = list(title = "", tickangle = -15),
        yaxis = list(title = "傳統位置", autorange = "reversed"),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)",
        font = list(family = "Noto Sans TC, sans-serif", size = 11, color = "#2c3e50"),
        margin = list(l = 60, r = 20, t = 30, b = 100)
      ) %>% config(displayModeBar = FALSE)
  })
  
  # ====== Position Stacked Bar ======
  output$position_bar <- renderPlotly({
    mp <- model_pipeline()
    if (is.null(mp)) return(NULL)

    bar_df <- mp$cross_long %>%
      group_by(Cluster) %>%
      mutate(pct = round(n / sum(n) * 100, 1)) %>%
      ungroup()

    pos_colors <- c("C" = "#E74C3C", "F" = "#F39C12", "G" = "#3498DB")
    positions <- sort(unique(bar_df$position))

    fig <- plot_ly(type = "bar")
    for (pos in positions) {
      sub  <- bar_df %>% filter(position == pos)
      col  <- if (pos %in% names(pos_colors)) pos_colors[[pos]] else "#95a5a6"
      fig  <- fig %>% add_trace(
        x = sub$Cluster,
        y = sub$pct,
        name = pos,
        marker = list(color = col, line = list(color = "white", width = 1)),
        text = paste0(sub$pct, "%"),
        textposition = "inside",
        insidetextanchor = "middle",
        textfont = list(color = "white", size = 12, family = "Noto Sans TC"),
        hovertemplate = paste0(
          "<b>", pos, "</b><br>分群: %{x}<br>佔比: %{y:.1f}%<extra></extra>"
        )
      )
    }

    fig %>%
      layout(
        barmode = "stack",
        xaxis = list(title = "", tickangle = -12,
                     gridcolor = "#ecf0f1", zeroline = FALSE),
        yaxis = list(title = "位置佔比 (%)", ticksuffix = "%",
                     gridcolor = "#ecf0f1", range = c(0, 100), zeroline = FALSE),
        legend = list(
          title = list(text = "傳統位置", font = list(size = 11)),
          orientation = "h", y = -0.22, font = list(size = 12)
        ),
        plot_bgcolor  = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)",
        font   = list(family = "Noto Sans TC, sans-serif", size = 12, color = "#2c3e50"),
        margin = list(l = 55, r = 20, t = 20, b = 110)
      ) %>%
      config(displayModeBar = FALSE)
  })

  # ====== DT 表格通用設定 ======
  dt_lang <- list(
    search = "搜尋:",
    info = "顯示第 _START_ 至 _END_ 筆,共 _TOTAL_ 筆",
    infoEmpty = "無資料",
    infoFiltered = "(已從 _MAX_ 筆過濾)",
    paginate = list(previous = "上一頁", `next` = "下一頁"),
    emptyTable = "無資料",
    zeroRecords = "找不到符合的資料",
    lengthMenu = "每頁 _MENU_ 筆"
  )
  
  make_dt <- function(df, page_length = 10, order_col = NULL) {
    opts <- list(
      pageLength = page_length,
      dom = 'frtip',
      scrollX = TRUE,
      language = dt_lang,
      autoWidth = FALSE
    )
    if (!is.null(order_col)) opts$order <- list(list(order_col, 'desc'))
    
    datatable(df, options = opts, rownames = FALSE, class = "stripe hover compact")
  }
  
  # 統一的群組著色器
  color_cluster_col <- function(dt) {
    dt %>% formatStyle(
      "Cluster",
      backgroundColor = styleEqual(names(CLUSTER_COLORS),
                                   paste0(CLUSTER_COLORS, "22")),
      fontWeight = "bold",
      borderLeft = styleEqual(names(CLUSTER_COLORS),
                              paste0("4px solid ", CLUSTER_COLORS))
    )
  }
  
  # ====== 各 DT 表格 ======
  output$k_quality_table <- renderDT({
    mp <- model_pipeline()
    if (is.null(mp)) return(NULL)
    
    make_dt(mp$k_quality_table, page_length = 7) %>%
      formatStyle("K",
                  target = "row",
                  backgroundColor = styleEqual(4, "#e8f8f3"),
                  fontWeight = styleEqual(4, "bold"))
  })
  
  output$cluster_summary_table <- renderDT({
    mp <- model_pipeline()
    if (is.null(mp)) return(NULL)
    color_cluster_col(make_dt(mp$cluster_summary, page_length = 5))
  })
  
  output$cluster_signature_table <- renderDT({
    mp <- model_pipeline()
    if (is.null(mp)) return(NULL)
    color_cluster_col(make_dt(mp$cluster_signature, page_length = 5))
  })
  
  output$cluster_means_table <- renderDT({
    mp <- model_pipeline()
    if (is.null(mp)) return(NULL)
    color_cluster_col(make_dt(mp$cluster_means, page_length = 5))
  })
  
  output$cluster_lift_table <- renderDT({
    mp <- model_pipeline()
    if (is.null(mp)) return(NULL)
    color_cluster_col(make_dt(mp$cluster_lift, page_length = 12))
  })
  
  output$cross_tab <- renderDT({
    mp <- model_pipeline()
    if (is.null(mp)) return(NULL)
    make_dt(mp$cross_table, page_length = 5)
  })
  
  output$player_detail_table <- renderDT({
    mp <- model_pipeline()
    if (is.null(mp)) return(NULL)
    
    df <- mp$df_result %>%
      select(name, Season, position, Cluster, gameId, numMinutes,
             points, assists, reboundsOffensive, reboundsDefensive) %>%
      mutate(across(where(is.numeric), ~round(., 2)))
    
    datatable(
      df,
      options = list(
        pageLength = 15,
        dom = 'frtip',
        scrollX = TRUE,
        order = list(list(6, 'desc')),  # 預設依得分排序
        language = dt_lang
      ),
      rownames = FALSE,
      class = "stripe hover compact",
      colnames = c("球員姓名", "賽季", "位置", "分群", "場次", "分鐘",
                   "PTS", "AST", "OREB", "DREB")
    ) %>% color_cluster_col()
  })
  
  # =============================================================
  # ====== 球員深度分析 (Scouting Tool) ======
  # =============================================================
  
  # ▶ 同步 selectizeInput 選項(篩選條件變動時)
  observe({
    mp <- model_pipeline()
    if (is.null(mp)) {
      updateSelectizeInput(session, "selected_player",
                           choices = character(0),
                           selected = character(0),
                           server = TRUE)
      return()
    }
    choices <- mp$df_result %>%
      arrange(name, Season) %>%
      pull(player_display) %>%
      unique()
    current <- isolate(input$selected_player)
    selected <- if (!is.null(current) && nzchar(current) && current %in% choices) {
      current
    } else if (length(choices) > 0) {
      choices[1]
    } else {
      character(0)
    }
    updateSelectizeInput(session, "selected_player",
                         choices = choices,
                         selected = selected,
                         server = TRUE,
                         options = list(
                           placeholder = "輸入球員姓名搜尋... (例:LeBron James、Stephen Curry)",
                           maxOptions = 50,
                           openOnFocus = TRUE
                         ))
  })
  
  # ▶ 已選球員的列(0 列或 1 列)
  selected_player_row <- reactive({
    mp <- model_pipeline()
    if (is.null(mp)) return(NULL)
    sel <- input$selected_player
    if (is.null(sel) || !nzchar(sel)) return(NULL)
    row <- mp$df_result %>% filter(player_display == sel)
    if (nrow(row) == 0) return(NULL)
    row[1, ]
  })
  
  # ▶ 五維百分位(對整個篩選樣本)
  df_with_dims <- reactive({
    mp <- model_pipeline()
    req(mp)
    mp$df_result %>%
      mutate(
        raw_scoring     = points,
        raw_playmaking  = assists,
        raw_rebounding  = reboundsOffensive + reboundsDefensive,
        raw_defense     = steals + blocks,
        raw_shooting    = (`FG%` + `3P%` + `FT%`) / 3,
        pct_scoring     = percent_rank(raw_scoring) * 100,
        pct_playmaking  = percent_rank(raw_playmaking) * 100,
        pct_rebounding  = percent_rank(raw_rebounding) * 100,
        pct_defense     = percent_rank(raw_defense) * 100,
        pct_shooting    = percent_rank(raw_shooting) * 100
      )
  })
  
  # ▶ Value boxes
  short_cluster_name <- function(s) gsub(" \\(.*\\)", "", s)
  
  output$player_cluster <- renderText({
    p <- selected_player_row()
    if (is.null(p)) return("— 請選擇球員 —")
    short_cluster_name(p$Cluster[1])
  })
  output$player_season <- renderText({
    p <- selected_player_row(); if (is.null(p)) return("—"); p$Season[1]
  })
  output$player_position <- renderText({
    p <- selected_player_row(); if (is.null(p)) return("—"); p$position[1]
  })
  output$player_games_mins <- renderText({
    p <- selected_player_row()
    if (is.null(p)) return("—")
    paste0(p$gameId[1], " / ", round(p$numMinutes[1], 1), " 分")
  })
  
  # ▶ 五維雷達(球員 vs 同群平均 vs 全體平均)
  output$player_radar <- renderPlotly({
    p <- selected_player_row()
    validate(need(!is.null(p), "請從上方搜尋並選擇一位球員,即可看到深度分析。"))
    
    ddim <- df_with_dims()
    req(ddim)
    
    pid <- p$row_idx
    pcluster <- p$Cluster
    
    prow <- ddim %>% filter(row_idx == pid)
    cavg <- ddim %>% filter(Cluster == pcluster) %>%
      summarise(across(starts_with("pct_"), ~mean(., na.rm = TRUE)))
    oavg <- ddim %>%
      summarise(across(starts_with("pct_"), ~mean(., na.rm = TRUE)))
    
    dim_names <- c("得分\nScoring", "組織\nPlaymaking", "籃板\nRebounding",
                   "防守\nDefense", "投籃效率\nShooting")
    
    player_vals  <- c(prow$pct_scoring, prow$pct_playmaking,
                      prow$pct_rebounding, prow$pct_defense, prow$pct_shooting)
    cluster_vals <- c(cavg$pct_scoring, cavg$pct_playmaking,
                      cavg$pct_rebounding, cavg$pct_defense, cavg$pct_shooting)
    overall_vals <- c(oavg$pct_scoring, oavg$pct_playmaking,
                      oavg$pct_rebounding, oavg$pct_defense, oavg$pct_shooting)
    
    player_color <- CLUSTER_COLORS[pcluster]
    if (is.na(player_color)) player_color <- "#2c3e50"
    
    plot_ly(type = "scatterpolar", mode = "lines+markers") %>%
      add_trace(
        r = c(overall_vals, overall_vals[1]),
        theta = c(dim_names, dim_names[1]),
        name = "全體平均",
        line = list(color = "#95a5a6", width = 1.2, dash = "dot"),
        marker = list(color = "#95a5a6", size = 5),
        fill = "none",
        hovertemplate = "%{theta}<br>全體平均: %{r:.1f}<extra></extra>"
      ) %>%
      add_trace(
        r = c(cluster_vals, cluster_vals[1]),
        theta = c(dim_names, dim_names[1]),
        name = "同群平均",
        line = list(color = player_color, width = 2, dash = "dash"),
        marker = list(color = player_color, size = 7),
        fill = "none",
        hovertemplate = "%{theta}<br>同群平均: %{r:.1f}<extra></extra>"
      ) %>%
      add_trace(
        r = c(player_vals, player_vals[1]),
        theta = c(dim_names, dim_names[1]),
        name = p$name,
        line = list(color = player_color, width = 3),
        marker = list(color = player_color, size = 11,
                      line = list(color = "white", width = 2)),
        fill = "toself",
        fillcolor = paste0(player_color, "45"),
        hovertemplate = paste0(p$name, "<br>%{theta}<br>百分位: %{r:.1f}<extra></extra>")
      ) %>%
      layout(
        polar = list(
          radialaxis = list(visible = TRUE, range = c(0, 100),
                            gridcolor = "#ecf0f1",
                            tickvals = c(25, 50, 75, 100),
                            ticksuffix = ""),
          angularaxis = list(gridcolor = "#ecf0f1",
                             tickfont = list(size = 11))
        ),
        showlegend = TRUE,
        legend = list(orientation = "h", y = -0.18, font = list(size = 11)),
        font = list(family = "Noto Sans TC, sans-serif", size = 11, color = "#2c3e50"),
        paper_bgcolor = "rgba(0,0,0,0)",
        margin = list(l = 60, r = 60, t = 30, b = 70)
      ) %>% config(displayModeBar = FALSE)
  })
  
  # ▶ 數據對照表:球員 vs 同群 vs 全體
  output$player_comparison <- renderDT({
    p <- selected_player_row()
    validate(need(!is.null(p), "請先選擇球員。"))
    
    mp <- model_pipeline(); req(mp)
    cluster_df <- mp$df_result %>% filter(Cluster == p$Cluster)
    
    tbl <- tibble(
      `指標` = FEATURES,
      `球員值` = sapply(FEATURES, function(m) round(p[[m]], 3)),
      `同群平均` = sapply(FEATURES, function(m) round(mean(cluster_df[[m]], na.rm = TRUE), 3)),
      `全體平均` = sapply(FEATURES, function(m) round(mean(mp$df_result[[m]], na.rm = TRUE), 3))
    ) %>%
      mutate(
        `vs 同群 (%)` = if_else(同群平均 == 0 | is.na(同群平均), NA_real_,
                                round((球員值 / 同群平均 - 1) * 100, 1)),
        `vs 全體 (%)` = if_else(全體平均 == 0 | is.na(全體平均), NA_real_,
                                round((球員值 / 全體平均 - 1) * 100, 1))
      )
    
    datatable(
      tbl,
      options = list(
        pageLength = 9, dom = 't', scrollX = TRUE, language = dt_lang
      ),
      rownames = FALSE, class = "stripe hover compact"
    ) %>%
      formatStyle(
        "vs 同群 (%)",
        color = styleInterval(c(-0.001, 0.001), c("#e74c3c", "#7f8c8d", "#27ae60")),
        fontWeight = "bold"
      ) %>%
      formatStyle(
        "vs 全體 (%)",
        color = styleInterval(c(-0.001, 0.001), c("#e74c3c", "#7f8c8d", "#27ae60")),
        fontWeight = "bold"
      )
  })
  
  # ▶ 群內排名
  output$player_ranks <- renderDT({
    p <- selected_player_row()
    validate(need(!is.null(p), "請先選擇球員。"))
    mp <- model_pipeline(); req(mp)
    
    cluster_df <- mp$df_result %>% filter(Cluster == p$Cluster)
    pid <- p$row_idx
    n_total <- nrow(cluster_df)
    
    rank_of <- function(values) {
      rnk <- rank(-values, ties.method = "min")
      target <- rnk[cluster_df$row_idx == pid]
      if (length(target) == 0) NA_integer_ else as.integer(target)
    }
    
    rows <- tibble(
      `指標` = c("得分 PTS", "助攻 AST", "籃板 REB", "抄截 STL", "阻攻 BLK", "防守 STL+BLK"),
      `球員值` = c(round(p$points, 2),
                  round(p$assists, 2),
                  round(p$reboundsOffensive + p$reboundsDefensive, 2),
                  round(p$steals, 2),
                  round(p$blocks, 2),
                  round(p$steals + p$blocks, 2)),
      `排名` = c(
        rank_of(cluster_df$points),
        rank_of(cluster_df$assists),
        rank_of(cluster_df$reboundsOffensive + cluster_df$reboundsDefensive),
        rank_of(cluster_df$steals),
        rank_of(cluster_df$blocks),
        rank_of(cluster_df$steals + cluster_df$blocks)
      ),
      `群內人數` = n_total
    ) %>%
      mutate(
        `百分位` = round((1 - (排名 - 1) / 群內人數) * 100, 1),
        `排名顯示` = paste0(排名, " / ", 群內人數)
      ) %>%
      select(`指標`, `球員值`, `排名` = `排名顯示`, `百分位`)
    
    datatable(
      rows,
      options = list(
        pageLength = 6, dom = 't', scrollX = TRUE, language = dt_lang,
        columnDefs = list(list(className = 'dt-center', targets = c(2, 3)))
      ),
      rownames = FALSE, class = "stripe hover compact"
    ) %>%
      formatStyle(
        "百分位",
        background = styleColorBar(c(0, 100), "#18bc9c40"),
        backgroundSize = "100% 88%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "left center",
        fontWeight = "bold"
      )
  })
  
  # ▶ 相似球員(歐氏距離 Top 10)
  output$similar_players_table <- renderDT({
    p <- selected_player_row()
    validate(need(!is.null(p), "請先選擇球員。"))
    mp <- model_pipeline(); req(mp)
    
    pid <- p$row_idx
    player_vec <- mp$X_scaled[pid, ]
    distances <- sqrt(rowSums(sweep(mp$X_scaled, 2, player_vec)^2))
    max_d <- max(distances[distances > 0], na.rm = TRUE)
    
    sim_tbl <- mp$df_result %>%
      mutate(
        distance = distances,
        similarity = round((1 - distance / max_d) * 100, 1)
      ) %>%
      filter(row_idx != pid) %>%
      arrange(distance) %>%
      head(10) %>%
      mutate(
        REB = round(reboundsOffensive + reboundsDefensive, 1),
        Cluster_short = short_cluster_name(Cluster)
      ) %>%
      transmute(
        `#` = row_number(),
        `球員` = name,
        `賽季` = Season,
        `位置` = position,
        `分群` = Cluster,
        PTS = round(points, 1),
        AST = round(assists, 1),
        REB,
        `距離` = round(distance, 2),
        `相似度` = similarity
      )
    
    datatable(
      sim_tbl,
      options = list(
        pageLength = 10, dom = 't', scrollX = TRUE, language = dt_lang,
        columnDefs = list(list(className = 'dt-center', targets = c(0, 3, 5, 6, 7, 8, 9)))
      ),
      rownames = FALSE, class = "stripe hover compact"
    ) %>%
      formatStyle(
        "分群",
        backgroundColor = styleEqual(names(CLUSTER_COLORS),
                                     paste0(CLUSTER_COLORS, "22")),
        fontWeight = "bold",
        borderLeft = styleEqual(names(CLUSTER_COLORS),
                                paste0("4px solid ", CLUSTER_COLORS))
      ) %>%
      formatStyle(
        "相似度",
        background = styleColorBar(c(0, 100), "#3498db40"),
        backgroundSize = "100% 88%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "left center",
        fontWeight = "bold"
      )
  })
  
  # =============================================================
  # ====== 下載處理器 (BOM 防亂碼) ======
  output$download_csv <- downloadHandler(
    filename = function() {
      season_clean <- gsub(" ", "_", input$season)
      paste0("NBA_Style_Clustering_", season_clean, ".csv")
    },
    content = function(file) {
      mp <- model_pipeline()
      if (!is.null(mp)) {
        export_df <- mp$df_result %>%
          select(name, Season, position, Cluster, gameId, numMinutes,
                 points, assists, reboundsOffensive, reboundsDefensive,
                 `FG%`, `3P%`, `FT%`)
        write_excel_csv(export_df, file)
      }
    }
  )
}

shinyApp(ui = ui, server = server)
