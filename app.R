library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(reshape2)
library(vcd)
library(MASS)
library(plotly)



df_mentah <- read.csv('preferensi_tsd.csv', sep = ';')
df_trans <- read.csv("numerik_normalisasi.csv")
df <- read.csv("preferensi_tsd2.csv", header = TRUE, sep = ",")
num_var <- c("Rerata", "Pengeluaran", "Frekuensi")


ui <- dashboardPage(
  dashboardHeader(
    title = "UAS KELOMPOK 12"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Anggota Kelompok", tabName = "anggota_kelompok", icon = icon("user-group")),
      menuItem("Project", tabName = "Project", icon = icon("book")),
      menuItem('Data', tabName = 'Data', icon = icon('database')),
      menuItem('Visualisasi & Statistik', tabName = 'Visualisasi', icon=icon('chart-simple'),
               menuSubItem('Visualisasi', tabName = 'EDA'),
               menuSubItem('Mentah vs Transformasi', tabName = 'mentah_trans'),
               menuSubItem('Data Transformasi', tabName = 'EDA_transformasi')),
      menuItem('Analisis Data', tabName = 'analisis', icon = icon('calculator'),
               menuSubItem('Uji Saphiro-Wilk', tabName = 'ujisw'),
               menuSubItem('Uji Korelasi Variabel Kuantitatif', tabName = 'ujirs'),
               #menuSubItem('Uji Tau-Kendall', tabName = 'ujitk'),
               menuSubItem('Uji Korelai Variabel Kategorik', tabName = 'ujic2'),
               menuSubItem('Regresi Logistik', tabName = 'regresi'))
    )
  ),
  dashboardBody(tabItems(
    tabItem(tabName = "anggota_kelompok",
            fluidPage(
              h2("Anggota Kelompok:"),
              h4("Aqila Hana Winanggoro(164221010)"),
              h4("Kyla Belva Queena(164221015)"),
              h4("Rashiqa Dewi Nariswari(164221044)"),
              h4("M. Naufal Izah Ramadhan(164221087)")
    )),
    tabItem(tabName = "Project", fluidPage(
            h2("ANALISIS PREFERENSI MAKANAN VIRAL TIKTOK PADA MAHASISWA TEKNOLOGI SAINS DATA"),
            
            h3("Latar Belakang:"),
            h4("Perkembangan peradaban sejalan dengan kemajuan teknologi, menjadikan kehidupan manusia sangat tergantung pada penggunaan teknologi, khususnya internet. Dalam era teknologi saat ini, internet dianggap sebagai kebutuhan esensial.  Tidak dapat dipungkiri bahwa internet sangat bermanfaat, yakni sebagai media informasi, hiburan, dan juga kepraktisannya. Pengguna internet di Indonesia sendiri selama kuartal pertama tahun 2019 hingga kuartal kedua tahun 2020 mencapai 73,7% atau 196,7 juta dari total penduduk Indonesia (APJII, 2020). Perkembangan internet yang semakin meningkat mendorong munculnya popularitas media sosial, salah satunya Tiktok."),
            
            h4("Aplikasi Tiktok pada tahun 2020 menjadi aplikasi terbanyak yang diunduh pada kedua pengguna (IOS dan apple), yakni mencapai 63,3 juta pengguna (Divani dkk., 2023). Berakar dari Cina, aplikasi ini menyalurkan kreativitas pengguna dalam bentuk audio visual sekaligus sebagai media hiburan kepada pengguna lainnya. Sebagian besar pengguna Tiktok terdiri dari remaja dan orang yang telah lulus dari perguruan tinggi sebab dipercaya kedua kelompok tersebut menghabiskan waktu yang signifikan menikmati video dan hiburan yang ditawarkan oleh platform Tiktok (Shengjie, 2021). Dampak masif lainnya penggunaan Tiktok terlihat dalam kemunculan tren-tren yang berkembang, yang sebagian besar dipicu oleh fenomena viral. Konten-konten yang menjadi viral dapat merubah dinamika platform dengan cepat, menciptakan tren yang menciptakan identitas budaya khusus di dalam Tiktok."),
            
            h4("Viral dalam kasus ini diyakini sebagai fenomena di dunia digital yang mencerminkan penyebaran informasi melalui berbagai media online secara cepat, menjadikannya populer dan menjadi topik pembicaraan di kalangan banyak orang (Bernatta dan Kartika, 2020). Fenomena ini terlihat dalam berbagai produk yang menjadi viral di Tiktok, antara lain makanan ringan, seperti crombolloni, dessert box, mochi, dan lain-lainnya. Menghadapi tren yang berkembang di TikTok, pengguna cenderung mengikuti arus untuk tetap terhubung dengan tren terbaru. Fenomena ini dapat mengakibatkan keputusan pembelian yang tidak terencana dan impulsif, hanya karena dipengaruhi oleh popularitasnya di platform tersebut."),
            
            h3("Sumber Data:"),
            h4("Dalam penelitian ini, kami dari kelompok 12 akan melakukan penelitian untuk menganalisis preferensi pembelian makanan viral TikTok pada mahasiswa Teknologi Sains Data Universitas Airlangga. Data ini kami peroleh secara mandiri dengan metode survey pengisian kuesioner formulir dengan media Google Form. Hasil yang kami peroleh berupa data kuantitatif dan juga kualitatif. Populasi yang kami ambil adalah Mahasiswa Teknologi Sains Data berjumlah 428 orang."),
            
            h3("Deskripsi Data:"),
            h4("Data preferensi pembelian makanan viral TikTok pada mahasiswa Teknologi Sains Data Universitas Airlangga diperoleh secara mandiri melalui penggunaan formulir kuesioner yang disebarkan melalui media Google Form. Hasil survei ini mencakup informasi baik secara kuantitatif maupun kualitatif."),
            
            h4("Populasi yang menjadi fokus penelitian ini adalah mahasiswa Teknologi Sains Data yang berjumlah 428 orang, dan dari populasi tersebut berhasil diambil sampel sebanyak 82 responden. Namun, setelah melalui proses tahap pre-processing, data yang digunakan dalam analisis ini mencakup 77 responden."),
            
            h3("Daftar Variabel:"),
            h4("Angkatan : angkatan responden berasal."),
            h4("Gender : gender dari responden."),
            h4("Alasan : penyebab mahasiswa TSD membeli makanan viral TikTok, apakah karena menarik, harganya murah, promo, rekomendasi teman, atau karena mengikuti trend saja."),
            h4("Skala : berapa kali mahasiswa membeli makanan viral, 1 untuk 1-2x, 2 untuk 3-4x, 3 untuk 5-6x, 4 untuk 7-8x, dan 5 untuk diatas 8x. Makanan merupakan macam-macam makanan viral Tiktok."),
            h4("Rerata : rata-rata harga makanan viral yang dibeli oleh mahasiswa."),
            h4("Pengeluaran : uang makan bulanan mahasiswa TSD"),
            h4("Frekuensi : berapa lama mahasiswa TSD menghabiskan waktu mereka untuk bermain TikTok.")
            
    )),
    tabItem(tabName = 'Data', fluidPage(
      h2("ANALISIS PREFERENSI MAKANAN VIRAL TIKTOK PADA MAHASISWA TEKNOLOGI SAINS DATA"),
      br(),
      DTOutput("table")
    )),
    tabItem(tabName = 'EDA', fluidPage(
      h3('Visualisasi'),
      br(),
      h2("Barchart"),
      selectInput("selected_ggplot", "Pilih Variabel", 
                  choices = c("Angkatan", "Gender", "Alasan", "Skala", "Makanan")),
      plotOutput("selected_ggplot_plot"),
      h3('Interpretasi'),
      verbatimTextOutput("interpretation_ggplot_text"),
      h2('Barchart Bivariat'),
      h3('Terhadap Skala Pembelian Makanan Viral'),
      selectInput("choice_bivbar", "Pilih Variabel",
                  choices = c("Gender", "Alasan")),
      plotOutput("bivbar_result"),
      h3('Interpretasi'),
      verbatimTextOutput("interpretation_bivbar_text"),
    )),
    tabItem(tabName = 'mentah_trans', fluidPage(
      h2("EDA Mentah vs Transformasi"),
      selectInput("selected_dataframe_summary", "pilih dataframe untuk menampilkan summary", choices = c("df_mentah", "df_trans")),
      verbatimTextOutput("summary_stats"),
      selectInput("selected_variable", "pilih variabel", choices = num_var),
      plotOutput("density_plot"),
      h3('Interpretasi'),
      p('Terlihat dari grafik, data sebelum dan sesudah transformasi tidak begitu signifikan perubahannya.')
    )),
    tabItem(tabName = 'EDA_transformasi', fluidPage(
      h2("Data Transformasi"),
      h2("Visualisasi Hubungan"),
      selectInput("selected_perbandingan", "Pilih Visualisasi Hubungan",
                  choices = c("Rerata dan Pengeluaran", "Rerata dan Frekuensi", "Rerata dan Skala")),
      plotOutput("plot_perbandingan"),
      h3("Interpretasi"),
      p("Dari ketiga scatter plot tersebut, variabel rerata tidak memiliki korelasi dengan semuanya. Artinya rata-rata harga makanan tidak memiliki hubungan dengan uang makan mahasiswa, frekuensi lama bermain tiktok, dan skala pembelian makanan viral."),
      h2("Boxplot"),
      selectInput("selected_boxplot", "Pilih Visualisasi Boxplot",
                  choices = c("Gender vs Frekuensi", "Alasan vs Frekuensi")),
      plotOutput("plot_boxplot"),
      verbatimTextOutput("interpretation_boxplot_text"),
      h2("Histogram & Density Plots"),
      selectInput("selected_variable_hist_density", "Pilih Variabel", 
                  choices = c("Rerata", "Frekuensi")),
      selectInput("selected_plot_type_hist_density", "Pilih Jenis Plot", 
                  choices = c("Histogram", "Density")),
      plotOutput("hist_density_plot"),
      h3('Interpretasi'),
      p('1. Pada rerata, jika dilihat dari density plot, distribusi rerata harga makanan viral TikTok tidak simetris, menunjukkan adanya kemiringan pada data. Ketinggian puncak distribusi berada di tengah, menandakan bahwa rerata harga makanan cenderung berkumpul di harga 20 hingga 30 ribu.'),
      p('2. Berdasarkan density plot dapat disimpulkan bahwa variabel Frekuensi memiliki sifat right skewed (positive), jadi data variabel frekuensi tidak simetris. Hal ini disebabkan karena sebagian besar mahasiswa TSD cenderung berada di nilai-nilai yang lebih rendah, sedangkan nilai-nilai yang tinggi menjadi semakin jarang. Artinya frekuensi penggunaan TikTok mahasiswa TSD rendah.'),
      h2("Heatmap Korelasi Variabel Numerik"),
      plotOutput("correlation_heatmap_plot"),
      h3('Interpretasi'),
      p('Pada heatmap di atas, terlihat bahwa variabel yang berkorelasi positif kuat, memiliki warna pink muda yang semakin merah. Sedangkan variabel yang berkorelasi negatif kuat, memiliki warna ungu muda yang semakin biru. Warna putih menunjukkan korelasi yang mendekati atau sama dengan 0.'),
      p('Berikut adalah 3 korelasi yang ada:'),
      p('1. Rerata dan Pengeluaran, memiliki bagian bewarna putih. Artinya tidak ada hubungan linier yang jelas antara variabel-variabel tersebut.'),
      p('2. Rerata dan Frekuensi, memiliki bagian bewarna pink muda. Artinya kedua variabel tersebut memiliki korelasi positif lemah, dimana jika satu variabel meningkat, variabel lainnya juga cenderung meningkat, namun korelasinya tidak begitu kuat.'),
      p('3. Pengeluaran dan Frekuensi, memiliki bagian bewarna ungu muda. Artinya kedua variabel tersebut memiliki korelasi negatif lemah, dimana jika satu variabel meningkat, variabel lainnya cenderung untuk menurun, namun keterkaitan ini tidak terlalu kuat.')
    )),
    tabItem(tabName = 'ujisw', fluidPage(
      h2("Uji Saphiro-Wilk"),
      selectInput("variable_to_sw", "Pilih Variabel", choices = c("Rerata", "Pengeluaran", "Frekuensi")),
      verbatimTextOutput("sw_test"),
      h3('Interpretasi'),
      verbatimTextOutput("interpretation_sw_text"),
      p('Dari uji korelasi spearman didapat kesimpulan bahwa variabel yang berhubungan hanya variabel rata-rata harga makanan dan frekuensi penggunaan tiktok dengan korelasi rendah dan arahnya positif')
    )),
    tabItem(tabName = 'ujirs', 
            fluidPage(
                h2("Uji Korelasi Rank Spearman"),
                selectInput("ujirs_choice", "Pilih Korelasi",
                            choices = c("Rerata vs Pengeluaran", "Rerata vs Frekuensi", "Pengeluaran vs Frekuensi")),
                verbatimTextOutput("ujirs_result"),
                h3('Interpretasi'),
                verbatimTextOutput("interpretation_rs_text"),
                br(),
                h2("Uji Korelasi Kendall"),
                selectInput("ujikendall_choice", "Select UjiKendall",
                            choices = c("Rerata vs Pengeluaran", "Rerata vs Frekuensi", "Pengeluaran vs Frekuensi")),
                verbatimTextOutput("ujikendall_result"),
                h3('Interpretasi'),
                verbatimTextOutput("interpretation_tk_text"),
              )),
              
             
      
    tabItem(tabName = 'ujic2', fluidPage(
      h2("Uji Chi-Squared"),
      selectInput("chi_square_choice", "Select Chi-Squared Test",
                  choices = c("Skala vs Gender", "Skala vs Alasan", "Skala vs Angkatan",
                              "Gender vs Alasan", "Gender vs Angkatan", "Alasan vs Angkatan")),
      verbatimTextOutput("chi_square_result"),
      h3('Interpretasi'),
      verbatimTextOutput("interpretation_cs_text"),
    )),
    tabItem(tabName = 'regresi', fluidPage(
      h2("Model Regresi Logistik"),
      selectInput("logistic_regression_choice", "Select Logistic Regression Model",
                  choices = c("Model 1: Skala ~ Rerata + Pengeluaran + Frekuensi",
                              "Model 2: Skala ~ Gender + Alasan")),
      verbatimTextOutput("logistic_regression_result"),
      h3('Interpretasi'),
      verbatimTextOutput("interpretation_regresi_text"),
      
    ))
  )
  )
  )


server <- function(input, output) {
  output$table <- renderDT({
    datatable(df_mentah, options = list(pageLength = 10))
  })
  
  num_var <- c("Rerata", "Pengeluaran", "Frekuensi")
  df_mentah$Pengeluaran <- as.numeric(gsub("[^0-9]", "", df_mentah$Pengeluaran))
  
  output$density_plot <- renderPlot({
    layout(matrix(c(1, 2), ncol = 2))  # Set the layout for two plots with different heights
    
    selected_var <- input$selected_variable
    
    # Plot density for df_mentah
    plot(density(df_mentah[[selected_var]]),
         main = paste("Density of", selected_var, "in df_mentah"),
         xlab = selected_var, ylab = "Density")
    
    # Plot density for df_trans
    if (selected_var %in% colnames(df_trans)) {
      plot(density(df_trans[[selected_var]]),
           main = paste("Density of", selected_var, "in df_trans"),
           xlab = selected_var, ylab = "Density")
    } else {
      warning(paste("Selected variable", selected_var, "does not exist in df_trans."))
    }
  })
  

  output$summary_stats <- renderPrint({
    selected_dataframe <- get(input$selected_dataframe_summary)
    
    if (is.data.frame(selected_dataframe)) {
      summary_stats <- summary(selected_dataframe)
      return(summary_stats)
    } else {
      warning("Selected dataframe is not valid.")
      return(NULL)
    }
  })
  observeEvent(input$str_button, {
    cat("Structure of df_mentah:\n")
    str(df_mentah)
    cat("df_trans structure:\n")
    str(df_trans)
  })
  
  observeEvent(input$str_2, {
    print('missing value mentah')
    print(sum(is.na(df_mentah)))
    print('missing value trans')
    print(colSums(is.na(df_trans)))
  })
  
  # Function to create the food frequency plot
  create_food_frequency_plot <- function(data) {
    makanan_pisah <- unlist(strsplit(data$Makanan, ", "))
    frekuensi_makanan <- table(makanan_pisah)
    df_makanan <- as.data.frame(frekuensi_makanan)
    
    ggplot(df_makanan, aes(x = makanan_pisah, y = Freq)) +
      geom_bar(stat = "identity", fill = "cornflowerblue", color = "black") +
      labs(x = "Makanan", y = "Frekuensi", title = "Frekuensi Makanan") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  # BARCHART
    output$selected_ggplot_plot <- renderPlot({
    selected_ggplot <- input$selected_ggplot
    
    if (selected_ggplot == "Angkatan") {
      ggplot(df, aes(x = Angkatan)) +
        geom_bar(fill = "red", color = "black") +
        labs(x = "Angkatan", y = "Frequency", title = "Angkatan Pembeli Makanan Viral")
    } else if (selected_ggplot == "Gender") {
      ggplot(df, aes(x = Gender)) +
        geom_bar(fill = "orange", color = "black") +
        labs(x = "Gender", y = "Frequency", title = "Gender Pembeli Makanan Viral")
    } else if (selected_ggplot == "Alasan") {
      ggplot(df, aes(x = Alasan)) +
        geom_bar(fill = "yellow", color = "black") +
        labs(x = "Alasan", y = "Frequency", title = "Alasan Membeli Makanan Viral")
    } else if (selected_ggplot == "Skala") {
      ggplot(df, aes(x = Skala)) +
        geom_bar(fill = "green", color = "black") +
        labs(x = "Skala", y = "Frequency", title = "Skala Pembelian Makanan Viral")
    } else if (selected_ggplot == "Makanan") {
      makanan_pisah <- unlist(strsplit(df$Makanan, ", "))
      frekuensi_makanan <- table(makanan_pisah)
      df_makanan <- as.data.frame(frekuensi_makanan)
      
      ggplot(df_makanan, aes(x = makanan_pisah, y = Freq)) +
        geom_bar(stat = "identity", fill = "blue", color = "black") +
        labs(x = "Makanan", y = "Frekuensi", title = "Frekuensi Makanan") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
    output$interpretation_ggplot_text <- renderText({
      selected_ggplot <- input$selected_ggplot
      
      interpretation_ggplot <- switch(
        selected_ggplot,
        "Angkatan" = "Angkatan 2022 adalah pembeli makanan viral TikTok terbanyak, sekitar 32 dari total 70 orang. Sedangkan angkatan 2020 merupakan pembeli paling sedikit.",
        "Gender" = "Pembeli makanan viral TikTok lebih banyak dari kalangan perempuan daripada laki-laki, yaitu sebanyak 43.",
        "Alasan" = "Sebagian besar mahasiswa TSD membeli makanan viral TikTok karena menarik, yaitu sebanyak 36 suara. Selanjutnya diikuti trend, rekomendasi teman, promo, dan terakhir paling sedikit karena harganya murah dengan total 2 suara.",
        "Skala" = "Skala pembelian makanan viral TikTok paling banyak adalah 1, yaitu 1 sampai 2x pembelian. Kemudian menurun sesuai jumlah skala yaitu 5. Hal ini mungkin karena mahasiswa TSD membeli makanan viral Tiktok hanya karena penasaran, mengikuti trend, atau rekomendasi teman saja.",
        "Makanan" = "Makanan viral favorit mahasiswa TSD adalah Bomboloni, dengan total 49 suara. Sedangkan Dalgona Coffee paling tidak favorit, hanya 1 dari 70 suara."
      )
      
      return(interpretation_ggplot)
    })
    output$hist_density_plot <- renderPlot({
      selected_variable <- input$selected_variable_hist_density
      selected_plot_type <- input$selected_plot_type_hist_density
      
      if (selected_variable == "Rerata") {
        if (selected_plot_type == "Histogram") {
          ggplot(df, aes(x = Rerata)) +
            geom_histogram(fill = "pink") +
            labs(title = "Harga Rerata Makanan Viral TikTok")
        } else if (selected_plot_type == "Density") {
          ggplot(df, aes(x = Rerata)) +
            geom_density(fill = "pink") +
            labs(title = "Harga Rerata Makanan Viral TikTok")
        }
      } else if (selected_variable == "Frekuensi") {
        if (selected_plot_type == "Histogram") {
          ggplot(df, aes(x = Frekuensi)) +
            geom_histogram(fill = "grey") +
            labs(title = "Frekuensi Penggunaan Tiktok Mahasiswa TSD")
        } else if (selected_plot_type == "Density") {
          ggplot(df, aes(x = Frekuensi)) +
            geom_density(fill = "grey") +
            labs(title = "Frekuensi Penggunaan Tiktok Mahasiswa TSD")
        }
      }
    })
    
    output$plot_perbandingan <- renderPlot({
      selected_visualization_perbandingan <- input$selected_perbandingan
      
      if (selected_visualization_perbandingan == "Rerata dan Pengeluaran") {
        ggplot(df_trans, aes(x = Rerata, y = Pengeluaran)) +
          geom_point(color = "purple") +
          labs(title = "Hubungan antara Rerata dan Pengeluaran")
      } else if (selected_visualization_perbandingan == "Rerata dan Frekuensi") {
        ggplot(df_trans, aes(x = Rerata, y = Frekuensi)) +
          geom_point(color = "grey") +
          labs(title = "Hubungan antara Rerata dan Frekuensi")
      } else if (selected_visualization_perbandingan == "Rerata dan Skala") {
        ggplot(df, aes(x = Rerata, y = Skala)) +
          geom_point(color = "green") +
          labs(title = "Hubungan antara Rerata dan Skala")
      } else if (selected_visualization_perbandingan == "Alasan vs Frekuensi"){
        ggplot(df, aes(x = Alasan, y = Frekuensi)) +
          geom_boxplot(fill = "yellow", alpha = 0.7) +
          labs(title = "Boxplot Alasan dengan Frekuensi")
      } else if (selected_visualization_perbandingan == "Gender vs Frekuensi"){
        ggplot(df, aes(x = Gender, y = Frekuensi)) +
          geom_boxplot(fill = "orange", alpha = 0.7) +
          labs(title = "Boxplot Gender dengan Frekuensi")
      }
    })
    
    
    output$bivbar_result <- renderPlot({
      selected_bivbar <- input$choice_bivbar
      if (selected_bivbar == "Gender") {
        ggplot(df,
               aes(x = Skala,
                   fill = Gender)) +
          geom_bar(position = "fill") +
          labs(y = "proportion", title = "Skala Pembelian Makanan Viral pada Gender")
        
      } else if (selected_bivbar == "Alasan") {
        ggplot(df,
               aes(x = Skala,
                   fill = Alasan)) +
          geom_bar(position = "fill") +
          labs(y = "proportion", title = "Skala Pembelian Makanan Viral pada Alasan")
        }
      
    })
    output$interpretation_bivbar_text <- renderText({
      selected_bivbar <- input$choice_bivbar
      
      interpretation <- switch(
        selected_bivbar,
        "Gender" = HTML("Dari segmented bar chart skala pembelian makanan viral pada Gender, skala 1, pembelian sebanyak 1-2 kali, memiliki pembeli perempuan yang lebih banyak, walaupun bedanya tipis dengan pembeli laki-laki, menunjukkan pembelian yang cukup seimbang.
Di skala 2, pembelian sebanyak 3-4 kali, <65% pembelinya adalah laki-laki sedangkan sisanya adalah perempuan.
Di skala 3, pembelian sebanyak 5-6 kali, sekitar 55% pembelinya adalah perempuan, sedangkan sisanya adalah laki-laki.
Sedangkan di skala 4 dan 5 semua pembelinya adalah perempuan.
Menunjukkan bahwa semua yang melakukan pembelian makanan viral di atas 5X adalah perempuan."),
        "Alasan" = HTML("Dari segmented bar chart skala pembelian makanan viral pada Alasan, di skala 1 sebagian besar melakukan pembelian karena menarik.
Di skala 2, terdapat keseimbangan pembelian makanan viral antara karena trend dan menarik.
Di skala 3, terdapat keseimbangan pembelian makanan viral antara karena rekomendasi teman dan menarik.
Di skala 4, 50% membeli karena promo, dan 50% lainnya membeli karena menarik, seimbang.
Di skala 5, semua mahasiswa membeli karena mengikuti trend.")
      )
      
      return(interpretation)
    })
    
    
    
    output$plot_boxplot <- renderPlot({
      selected_visualization_perbandingan <- input$selected_boxplot
      
      if (selected_visualization_perbandingan == "Alasan vs Frekuensi"){
        ggplot(df, aes(x = Alasan, y = Frekuensi)) +
          geom_boxplot(fill = "yellow", alpha = 0.7) +
          labs(title = "Boxplot Alasan dengan Frekuensi")
      } else if (selected_visualization_perbandingan == "Gender vs Frekuensi"){
        ggplot(df, aes(x = Gender, y = Frekuensi)) +
          geom_boxplot(fill = "orange", alpha = 0.7) +
          labs(title = "Boxplot Gender dengan Frekuensi")
      }
    })
    output$interpretation_boxplot_text <- renderText({
      selected_boxplot <- input$selected_boxplot
      
      interpretation <- switch(
        selected_boxplot,
        "Gender vs Frekuensi" = HTML("Secara visual, laki-laki terlihat interquartile (Q3-Q1) lebih luas dibandingkan dengan perempuan.
Maka dapat dikatakan data frekuensi pembelian laki-laki lebih bervariasi.
Median laki-laki dan perempuan sama. Pada perempuan terdapat data pencilan.
Hal tersebut mengindikasi terdapat 1 mahasiswa yang memiliki frekuensi pembelian jauh dibandingkan dengan mahasiswa lainnya."),
        "Alasan vs Frekuensi" = HTML("Tidak terdapat outlier di ke-5 alasan yang ada.
Jika melihat panjang kotak dan posisi median (garis hitam), promo/diskon khusus memiliki penyebaran data yang paling banyak dan memiliki kemiringan yang positif.
Sedangkan alasan harga murah memiliki penyebaran data yang paling sedikit, tetapi berdistribusi normal.")
      )
      
      return(interpretation)
    })
    
    #HEATMAP
    output$correlation_heatmap_plot <- renderPlot({
      numerical_columns <- c("Rerata", "Pengeluaran", "Frekuensi")
      
      # Menghitung matriks korelasi
      correlation_matrix <- cor(df_trans[numerical_columns], method = "spearman")
      
      # Membuat plot heatmap
      heatmap_data <- melt(correlation_matrix)
      ggplot(heatmap_data, aes(Var1, Var2, fill = value)) +
        geom_tile(color = "white") +
        scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, limit = c(-1,1), space = "Lab", name="Correlation") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1)) +
        coord_fixed()
    })
    
    # Mengubah variabel skala dan angkatan menjadi tipe data factor
    df1 <- df
    df1$Angkatan <- as.character(df1$Angkatan)
    df1$Skala <- factor(df1$Skala)
    df_baru <- data.frame(
      Angkatan = df1$Angkatan,
      Gender = df1$Gender,
      Alasan = df1$Alasan,
      Skala = df1$Skala,
      Rerata = df_trans$Rerata,
      Pengeluaran = df_trans$Pengeluaran,
      Frekuensi = df_trans$Frekuensi
    )
    
    observe({
      variable <- switch(
        input$variable_to_sw,
        Rerata = df_baru$Rerata,
        Pengeluaran = df_baru$Pengeluaran,
        Frekuensi = df_baru$Frekuensi
      )
      
      test_result <- shapiro.test(variable)
      output$sw_test <- renderPrint({
        return(test_result)
      })
    })
    
    
    observe({
      selected_ujirs <- input$ujirs_choice
      
      req(selected_ujirs)
      
      if (selected_ujirs == "Rerata vs Pengeluaran") {
        ujirs_result <- suppressWarnings(cor.test(df_baru$Rerata, df_baru$Pengeluaran, method = "spearman"))
      } else if (selected_ujirs == "Rerata vs Frekuensi") {
        ujirs_result <- suppressWarnings(cor.test(df_baru$Rerata, df_baru$Frekuensi, method = "spearman"))
      } else if (selected_ujirs == "Pengeluaran vs Frekuensi") {
        ujirs_result <- suppressWarnings(cor.test(df_baru$Pengeluaran, df_baru$Frekuensi, method = "spearman"))
      }
      
      output$ujirs_result <- renderPrint(ujirs_result)
    })
    output$interpretation_rs_text <- renderText({
      selected_rs <- input$ujirs_choice
      
      interpretation <- switch(
        selected_rs,
        "Rerata vs Pengeluaran" = HTML("Pada uji korelasi spearman diketahui bahwa variabel rata-rata harga makanan dan pengeluaran untuk makan dalam sebulan tidak memiliki hubungan.
Hal ini dikarenakan p-value (0.7283) > alpha (0.1)."),
        "Pengeluaran vs Frekuensi" = HTML("Pada uji korelasi spearman diketahui bahwa variabel pengeluaran untuk makan dalam sebulan dan frekuensi penggunaan TikTok dalam sehari tidak memiliki hubungan.
Hal ini dikarenakan p-value (0.5051) > alpha (0.1)."),
        "Rerata vs Frekuensi" = HTML("Pada uji korelasi spearman diketahui bahwa variabel rata-rata harga makanan dan frekuensi penggunaan TikTok dalam sehari memiliki hubungan yang rendah dan arahnya positif. 
Hal ini dikarenakan p-value (0.01258) < alpha (0.1), dan nilai rho 0.283 menandakan kekuatan dan arah hubungan.")
      )
      
      return(interpretation)
    })
    
    observe({
      selected_ujikendall <- input$ujikendall_choice
      
      req(selected_ujikendall)
      
      if (selected_ujikendall == "Rerata vs Pengeluaran") {
        ujikendall_result <- cor.test(df_baru$Rerata, df_baru$Pengeluaran, method = "kendall")
      } else if (selected_ujikendall == "Rerata vs Frekuensi") {
        ujikendall_result <- cor.test(df_baru$Rerata, df_baru$Frekuensi, method = "kendall")
      } else if (selected_ujikendall == "Pengeluaran vs Frekuensi") {
        ujikendall_result <- cor.test(df_baru$Pengeluaran, df_baru$Frekuensi, method = "kendall")
      }
      
      output$ujikendall_result <- renderPrint(ujikendall_result)
    })
    output$interpretation_tk_text <- renderText({
      selected_tk <- input$ujikendall_choice
      
      interpretation <- switch(
        selected_tk,
        "Rerata vs Pengeluaran" = HTML("Pada uji korelasi tau kendall diketahui bahwa variabel rata-rata harga makanan dan pengeluaran untuk makan dalam sebulan tidak memiliki hubungan.
Hal ini dikarenakan p-value (0.7172) > alpha (0.1)."),
        "Pengeluaran vs Frekuensi" = HTML("Pada uji korelasi tau kendall diketahui bahwa variabel pengeluaran untuk makan dalam sebulan dan frekuensi penggunaan TikTok dalam sehari tidak memiliki hubungan.
Hal ini dikarenakan p-value (0.5325) > alpha (0.1)."),
        "Rerata vs Frekuensi" = HTML("Pada uji korelasi tau kendall diketahui bahwa variabel rata-rata harga makanan dan frekuensi penggunaan TikTok dalam sehari memiliki hubungan yang rendah dan arahnya positif.
Hal ini dikarenakan p-value (0.00823) < alpha (0.1)), dan nilai tau 0.235 menandakan kekuatan dan arah hubungan.")
      )
      
      return(interpretation)
    })
    
    observe({
      selected_chi_square <- input$chi_square_choice
      
      req(selected_chi_square)
      
      if (selected_chi_square == "Skala vs Gender") {
        chi_square_result <- chisq.test(table(df_baru$Skala, df_baru$Gender))
      } else if (selected_chi_square == "Skala vs Alasan") {
        chi_square_result <- chisq.test(table(df_baru$Skala, df_baru$Alasan))
      } else if (selected_chi_square == "Skala vs Angkatan") {
        chi_square_result <- chisq.test(table(df_baru$Skala, df_baru$Angkatan))
      } else if (selected_chi_square == "Gender vs Alasan") {
        chi_square_result <- chisq.test(table(df_baru$Gender, df_baru$Alasan))
      } else if (selected_chi_square == "Gender vs Angkatan") {
        chi_square_result <- chisq.test(table(df_baru$Gender, df_baru$Angkatan))
      } else if (selected_chi_square == "Alasan vs Angkatan") {
        chi_square_result <- chisq.test(table(df_baru$Alasan, df_baru$Angkatan))
      }
      
      output$chi_square_result <- renderPrint(chi_square_result)
    })
    output$interpretation_cs_text <- renderText({
      selected_cs <- input$chi_square_choice
      
      interpretation <- switch(
        selected_cs,
        "Skala vs Gender" = "Pada uji korelasi chi square diketahui bahwa variabel skala beli makanan viral dengan gender tidak memiliki hubungan. Hal ini dikarenakan p-value (0.3439) > alpha (0.1).",
        "Skala vs Alasan" = "Pada uji korelasi chi square diketahui bahwa variabel skala beli makanan viral dengan alasan membeli tidak memiliki hubungan. Hal ini dikarenakan p-value (0.557) > alpha (0.1).",
        "Skala vs Angkatan" = "Pada uji korelasi chi square diketahui bahwa variabel skala beli makanan viral dengan angkatan memiliki hubungan. Hal ini dikarenakan p-value (0.00733) < alpha (0.1).",
        "Gender vs Alasan" = "Pada uji korelasi chi square diketahui bahwa variabel gender dengan alasan membeli tidak memiliki hubungan. Hal ini dikarenakan p-value (0.1939) > alpha (0.1).",
        "Gender vs Angkatan" = "Pada uji korelasi chi square diketahui bahwa variabel gender dengan angkatan tidak memiliki hubungan. Hal ini dikarenakan p-value (0.7156) > alpha (0.1).",
        "Alasan vs Angkatan" = "Pada uji korelasi chi square diketahui bahwa variabel alasan membeli dengan angkatan tidak memiliki hubungan. Hal ini dikarenakan p-value (0.3174) > alpha (0.1)."
      ) 
      
      return(interpretation)
    })
    
    observe({
      selected_logistic_regression <- input$logistic_regression_choice
      
      req(selected_logistic_regression)
      
      if (selected_logistic_regression == "Model 1: Skala ~ Rerata + Pengeluaran + Frekuensi") {
        model <- polr(Skala ~ Rerata + Pengeluaran + Frekuensi, data = df_baru, Hess = TRUE)
      } else if (selected_logistic_regression == "Model 2: Skala ~ Gender + Alasan") {
        model <- polr(Skala ~ Gender + Alasan, data = df_baru, Hess = TRUE)
      }
      
      output$logistic_regression_result <- renderPrint({
        summary(model)
      })
    })
    output$interpretation_regresi_text <- renderText({
      selected_regresi <- input$logistic_regression_choice
      
      interpretation <- switch(
        selected_regresi,
        "Model 1: Skala ~ Rerata + Pengeluaran + Frekuensi" = HTML('Nilai koefisien Rerata adalah 0.605 yang artinya setiap ada kenaikan satu satuan rerata harga makanan viral, maka akan meningkatkan kemungkinan bertambahnya skala beli makanan viral sebesar 0.605.
Nilai koefisien Pengeluaran adalah 0.159 yang artinya setiap ada kenaikan satu satuan pengeluaran untuk makan dalam sebulan, maka akan meningkatkan kemungkinan bertambahnya skala beli makanan viral sebesar 0.159.
Nilai koefisien Frekuensi adalah 0.192 yang artinya setiap ada kenaikan satu jam penggunaan TikTok, maka akan meningkatkan kemungkinan bertambahnya skala beli makanan viral sebesar 0.192.'),
        "Model 2: Skala ~ Gender + Alasan" = HTML('Nilai koefisien Gender perempuan adalah 1.080 yang artinya gender perempuan berkemungkinan untuk bertambah skala beli makanan viralnya sebesar 1.080. Dapat dikatakan juga bahwa perempuan lebih berkemungkinan bertambah skala belinya dibandingkan laki-laki.
Nilai koefisien untuk Alasan menarik, Alasan Promo/diskon khusus, Alasan rekomendasi teman, dan Alasan mengikuti trend masing-masing adalah 0.250, 2.126, 0.948, 1.098. Hal ini dapat diartikan bahwa Alasan yang paling berpengaruh terhadap kenaikan skala beli makanan viral adalah karena adanya promo/diskon khusus dan mengikuti trend/fomo, yaitu sebesar 2.126 dan 1.098.')
        )
      
      return(interpretation)
    })
    
}



shinyApp(ui, server)
