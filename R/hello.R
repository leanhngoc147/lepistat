# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# Tạo data frame mẫu về dữ liệu bệnh nhân
create_sample_data <- function(n = 200) {
  # Tạo biến giới tính
  gender <- sample(c(1, 2), n, replace = TRUE, prob = c(0.45, 0.55))
  attr(gender, "labels") <- c("Nam" = 1, "Nữ" = 2)
  attr(gender, "label") <- "Giới tính"

  # Tạo biến nhóm tuổi
  age_group <- sample(c(1, 2, 3), n, replace = TRUE, prob = c(0.3, 0.5, 0.2))
  attr(age_group, "labels") <- c("18-40" = 1, "41-60" = 2, ">60" = 3)
  attr(age_group, "label") <- "Nhóm tuổi"

  # Tạo biến tuổi thực dựa trên nhóm tuổi
  age <- numeric(n)
  age[age_group == 1] <- round(runif(sum(age_group == 1), 18, 40))
  age[age_group == 2] <- round(runif(sum(age_group == 2), 41, 60))
  age[age_group == 3] <- round(runif(sum(age_group == 3), 61, 85))
  attr(age, "label") <- "Tuổi"

  # Tạo biến BMI với một số missing
  bmi <- rnorm(n, mean = 23 + (gender == 2) * 1, sd = 3.5)
  bmi[sample(1:n, 15)] <- NA # Tạo 15 giá trị NA ngẫu nhiên
  attr(bmi, "label") <- "Chỉ số khối cơ thể (BMI)"

  # Tạo biến huyết áp tâm thu
  sbp <- rnorm(n, mean = 120 + age_group * 5, sd = 15)
  sbp[sample(1:n, 10)] <- NA # Tạo 10 giá trị NA ngẫu nhiên
  attr(sbp, "label") <- "Huyết áp tâm thu (mmHg)"

  # Tạo biến cholesterol
  chol <- rnorm(n, mean = 180 + age_group * 10, sd = 30)
  chol[sample(1:n, 12)] <- NA # Tạo 12 giá trị NA ngẫu nhiên
  attr(chol, "label") <- "Cholesterol (mg/dL)"

  # Tạo biến tình trạng bệnh
  disease <- sample(c(0, 1, 2), n, replace = TRUE, prob = c(0.6, 0.3, 0.1))
  attr(disease, "labels") <- c("Không có bệnh" = 0, "Tiền sử bệnh" = 1, "Đang mắc bệnh" = 2)
  attr(disease, "label") <- "Tình trạng bệnh"

  # Tạo biến nhóm máu
  blood_type <- sample(c("A", "B", "AB", "O"), n, replace = TRUE, prob = c(0.3, 0.2, 0.1, 0.4))
  blood_type[sample(1:n, 8)] <- NA # Tạo 8 giá trị NA ngẫu nhiên
  attr(blood_type, "label") <- "Nhóm máu"

  # Tạo data frame
  df <- data.frame(
    gender = gender,
    age_group = age_group,
    age = age,
    bmi = bmi,
    sbp = sbp,
    chol = chol,
    disease = disease,
    blood_type = blood_type
  )

  return(df)
}
des <- function(data, gt = TRUE, vie = FALSE) {
  # Kiểm tra và nạp các gói cần thiết
  if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr", dependencies = TRUE)
  if (!requireNamespace("gt", quietly = TRUE)) install.packages("gt", dependencies = TRUE)
  library(dplyr)
  library(gt)

  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }

  # Lấy tên biến
  var_names <- names(data)

  # Lấy kiểu dữ liệu của từng biến
  var_types <- sapply(data, class)

  # Nếu các biến có nhãn, sử dụng hàm attr để lấy nhãn
  var_labels <- sapply(var_names, function(x) {
    label <- attr(data[[x]], "label")
    if (is.null(label)) {
      label <- ""
    }
    return(label)
  })

  # Tạo một dataframe để hiển thị kết quả
  summary_df <- tibble(
    variable_name = var_names,
    variable_label = var_labels,
    type = var_types
  )

  # Nếu vie = TRUE, sử dụng tiếng Việt
  if (vie) {
    colnames(summary_df) <- c("ten_bien", "nhan_bien", "kieu_du_lieu")
  } else {
    colnames(summary_df) <- c("variable_name", "variable_label", "type")
  }

  # Nếu gt = TRUE, sử dụng gói gt để tạo bảng
  if (gt) {
    summary_table <- gt(summary_df) %>%
      tab_header(
        title = if (vie) "Tóm tắt dữ liệu" else "Data Summary"
      ) %>%
      cols_label(
        .list = if (vie) {
          list(
            ten_bien = "Tên biến",
            nhan_bien = "Nhãn biến",
            kieu_du_lieu = "Kiểu dữ liệu"
          )
        } else {
          list(
            variable_name = "Variable Name",
            variable_label = "Variable Label",
            type = "Type"
          )
        }
      )

    return(summary_table)
  } else {
    return(summary_df)
  }
}

thuviendata <- function(data) {
  # Kiểm tra và cài đặt gói cần thiết
  if (!require(sjlabelled)) install.packages("sjlabelled")
  library(sjlabelled)
  if (!require(gt)) install.packages("gt")
  library(gt)
  variable_names <- names(data)

  data_dictionary <- data.frame(
    Variable_name = character(),
    Variable_values = character(),
    Variable_label = character(),
    stringsAsFactors = FALSE
  )

  for (var in variable_names) {
    variable <- data[[var]]
    var_label <- get_label(variable)
    if (is.null(var_label) || var_label == "") {
      var_label <- ""
    }

    # Lấy giá trị mã hóa của biến
    value_labels <- get_labels(variable, values = "p", attr.only = FALSE)
    if (!is.null(value_labels) && length(value_labels) > 0) {
      # Biến có nhãn giá trị
      values_desc <- paste0(names(value_labels), "=", value_labels, collapse = "; ")
    } else if (is.factor(variable)) {
      # Biến là factor nhưng không có nhãn giá trị
      levels_labels <- levels(variable)
      codes <- seq_along(levels_labels)
      values_desc <- paste0(codes, "=", levels_labels, collapse = "; ")
    } else if (is.numeric(variable)) {
      # Biến số
      values_desc <- paste("Numeric (N =", length(variable), "; from:",
                           min(variable, na.rm=TRUE), "to", max(variable, na.rm=TRUE), ")")
    } else {
      # Các loại biến khác
      unique_vals <- unique(variable)
      if (length(unique_vals) <= 10) {
        values_desc <- paste(unique_vals, collapse = "; ")
      } else {
        values_desc <- "Various values"
      }
    }

    data_dictionary <- rbind(data_dictionary, data.frame(
      Variable_name = var,
      Variable_values = values_desc,
      Variable_label = var_label,
      stringsAsFactors = FALSE
    ))
  }

  # Sử dụng gt để tạo bảng
  data_dictionary_gt <- gt(data_dictionary)

  return(data_dictionary_gt)
}
# Hàm thống kê mô tả
thongkemota <- function(data, by = NULL, digits = 1, title = "**Bảng thống kê mô tả**", mean_sd_vars = NULL, median_iqr_vars = NULL, bold_var = TRUE) {
  # Tích hợp các thư viện cần thiết
  if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
  if (!requireNamespace("haven", quietly = TRUE)) install.packages("haven")
  if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")
  if (!requireNamespace("broom", quietly = TRUE)) install.packages("broom")
  if (!requireNamespace("gt", quietly = TRUE)) install.packages("gt")

  library(dplyr)
  library(haven)
  library(tidyr)
  library(broom)
  library(gt)

  # Chuẩn bị dữ liệu
  data <- data %>% ungroup()

  # Chuyển đổi `by` và `include` thành tên biến
  by <- deparse(substitute(by))
  include <- colnames(data)

  if (!is.null(by) && by %in% include) {
    include <- setdiff(include, by)
  }

  # Hàm tạo metadata
  generate_metadata <- function(data, digits = 1) {
    columns <- colnames(data)
    meta_data <- data.frame(
      variable = columns,
      summary_type = sapply(data, function(col) {
        if (is.numeric(col)) return("continuous")
        else return("categorical")
      }),
      label = sapply(columns, function(col) {
        if (!is.null(attr(data[[col]], "label"))) {
          return(attr(data[[col]], "label"))
        } else {
          return(col)
        }
      }),
      stringsAsFactors = FALSE
    )

    meta_data$digits <- digits
    return(meta_data)
  }

  meta_data <- generate_metadata(data, digits)
  descriptive_table <- list()

  for (i in seq_len(nrow(meta_data))) {
    col <- meta_data$variable[i]
    label <- meta_data$label[i]

    if (meta_data$summary_type[i] == "continuous") {
      if (!is.null(mean_sd_vars) && col %in% mean_sd_vars) {
        mean_val <- sprintf(paste0("%.", digits, "f"), mean(data[[col]], na.rm = TRUE))
        std_dev <- sprintf(paste0("%.", digits, "f"), sd(data[[col]], na.rm = TRUE))
        descriptive_table <- append(descriptive_table, list(c(paste(label, "*"), paste0(mean_val, " ± ", std_dev), "")))
      } else if (!is.null(median_iqr_vars) && col %in% median_iqr_vars) {
        median_val <- sprintf(paste0("%.", digits, "f"), median(data[[col]], na.rm = TRUE))
        q1 <- sprintf(paste0("%.", digits, "f"), quantile(data[[col]], 0.25, na.rm = TRUE))
        q3 <- sprintf(paste0("%.", digits, "f"), quantile(data[[col]], 0.75, na.rm = TRUE))
        descriptive_table <- append(descriptive_table, list(c(paste(label, "**"), paste0(median_val, " (", q1, " - ", q3, ")"), "")))
      } else {
        # Kiểm tra phân phối chuẩn để quyết định loại báo cáo
        shapiro_test <- shapiro.test(data[[col]][!is.na(data[[col]])])
        if (shapiro_test$p.value > 0.05) {  # Có phân phối chuẩn
          mean_val <- sprintf(paste0("%.", digits, "f"), mean(data[[col]], na.rm = TRUE))
          std_dev <- sprintf(paste0("%.", digits, "f"), sd(data[[col]], na.rm = TRUE))
          descriptive_table <- append(descriptive_table, list(c(paste(label, "*"), paste0(mean_val, " ± ", std_dev), "")))
        } else {  # Không có phân phối chuẩn
          median_val <- sprintf(paste0("%.", digits, "f"), median(data[[col]], na.rm = TRUE))
          q1 <- sprintf(paste0("%.", digits, "f"), quantile(data[[col]], 0.25, na.rm = TRUE))
          q3 <- sprintf(paste0("%.", digits, "f"), quantile(data[[col]], 0.75, na.rm = TRUE))
          descriptive_table <- append(descriptive_table, list(c(paste(label, "**"), paste0(median_val, " (", q1, " - ", q3, ")"), "")))
        }
      }
    } else {
      # Nếu là biến phân loại
      freq_table <- data %>%
        count(!!sym(col)) %>%
        mutate(Frequency = round(n / sum(n) * 100, digits)) %>%
        mutate(Frequency = format(Frequency, nsmall = digits))

      descriptive_table <- append(descriptive_table, list(c(label, "", "")))

      for (row in 1:nrow(freq_table)) {
        descriptive_table <- append(descriptive_table, list(c(paste0("  ", as.character(freq_table[[col]][row])), freq_table$n[row], freq_table$Frequency[row])))
      }
    }
  }

  descriptive_table <- do.call(rbind, descriptive_table)
  colnames(descriptive_table) <- c("Đặc điểm", "Tần số (n)", "Tỉ lệ (%)")

  # Tạo bảng sử dụng gt
  gt_table <- gt(as.data.frame(descriptive_table)) %>%
    tab_header(
      title = md(title)
    ) %>%
    tab_style(
      style = list(
        cell_text(indent = px(20), align = "left")
      ),
      locations = cells_body(
        columns = vars(`Đặc điểm`)
      )
    ) %>%
    tab_style(
      style = cell_text(indent = px(0), align = "left", weight = ifelse(bold_var, "bold", "normal")),
      locations = cells_body(
        columns = vars(`Đặc điểm`),
        rows = !grepl("^  ", descriptive_table[, "Đặc điểm"])  # Áp dụng cho các hàng không có indent
      )
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels(
        columns = everything()
      )
    ) %>%
    cols_align(
      align = "center",
      columns = vars(`Tần số (n)`, `Tỉ lệ (%)`)
    ) %>%
    cols_label(
      `Đặc điểm` = "Đặc điểm",
      `Tần số (n)` = "Tần số (n)",
      `Tỉ lệ (%)` = "Tỉ lệ (%)"
    ) %>%
    tab_source_note(
      source_note = md("Chú thích: * trung bình ± độ lệch chuẩn, ** trung vị (khoảng tứ phân vị)")
    )

  return(gt_table)
}


my_theme1 <-
  my_theme <-
  list(
    "pkgwide-fn:pvalue_fun" = function(x) style_pvalue(x, digits = 3),
    "pkgwide-fn:prependpvalue_fun" = function(x) style_pvalue(x, digits = 3, prepend_p = TRUE),
    "tbl_summary-str:continuous_stat" = "{mean} ({sd})",
    "tbl_summary-str:categorical_stat" = "{n} ({p})"
  )


my_theme2 <-
  list(
    "pkgwide-fn:pvalue_fun" = function(x) style_pvalue(x, digits = 3),
    "pkgwide-fn:prependpvalue_fun" = function(x) style_pvalue(x, digits = 3, prepend_p = TRUE),
    "tbl_summary-str:continuous_stat" = "{mean}±{sd}",
    "tbl_summary-str:categorical_stat" = "{n} ({p}%)"
  )

RR1_cross <- function(data){
  data <- data %>%
    mutate(N_event =     as.numeric(str_replace(stat_1,"\\([\\d,\\.]+%\\)","")),
           N_nonevent =  as.numeric(str_replace(stat_2,"\\([\\d,\\.]+%\\)","")),
           N_total=N_event + N_nonevent)
  data <- data %>%
    mutate(Risk=N_event/(N_event+N_nonevent),baseRisk=Risk[2]) %>%
    mutate (RR=style_ratio(Risk/baseRisk))
}

RR_smry <- function(data, variable, by, ...){
  rr1 <-  epitools::riskratio(data[[by]],data[[variable]],rev="rows")
  stat = str_glue(style_ratio(rr1$measure[[2,1]]),
                  " (", style_ratio(rr1$measure[[2,2]]),",",style_ratio(rr1$measure[[2,3]]),")")
}


RR_cross <- function(data){
  data %>%
    mutate(N_event =     as.numeric(str_replace(stat_1,"\\([\\d,\\.]+%\\)","")),
           N_nonevent =  as.numeric(str_replace(str_replace(stat_2,",",""),"\\([\\d,\\.]+%\\)","")),
           N_total=N_event + N_nonevent,
           conf_factor=exp(1.96*sqrt(1/N_event-1/N_total+1/N_event[2]-1/N_total[2]))) %>%
    mutate(Risk=N_event/(N_event+N_nonevent),baseRisk=Risk[2]) %>%
    mutate (RRi=Risk/baseRisk,
            str_RRi=str_c(style_ratio(RRi),"(",style_ratio(RRi/conf_factor),"-",style_ratio(RRi*conf_factor),")"),
            RR=ifelse(row_number()==2,style_ratio(RRi),str_RRi))
}




OR_smry <- function(data, variable, by, ...){
  rr1 <-  epitools::riskratio.fisher(data[[by]],data[[variable]],rev="rows")
  stat = str_glue(style_ratio(rr1$measure[[2,1]]),
                  " (", style_ratio(rr1$measure[[2,2]]),",",style_ratio(rr1$measure[[2,3]]),")")
}


ORw_smry <- function(data, variable, by, ...){
  rr1 <-  epitools::riskratio.wald(data[[by]],data[[variable]])
  stat = str_glue(style_ratio(rr1$measure[[2,1]]),
                  " (", style_ratio(rr1$measure[[2,2]]),",",style_ratio(rr1$measure[[2,3]]),")")
}

ORw_cross <- function(data){
  data %>%
    mutate(N_event =     as.numeric(str_replace(stat_2,"\\([\\d,\\.]+%\\)","")),
           N_nonevent =  as.numeric(str_replace(stat_1,"\\([\\d,\\.]+%\\)","")),
           N_total=N_event + N_nonevent,
           conf_factor=exp(1.96*sqrt(1/N_event+1/N_nonevent+1/N_event[2]+1/N_nonevent[2]))) %>%
    mutate(Odds=N_event/N_nonevent,baseOdds=Odds[3]) %>%
    mutate (ORi=Odds/baseOdds,
            str_ORi=str_c(style_ratio(ORi),"(",style_ratio(ORi/conf_factor),"-",style_ratio(ORi*conf_factor),")"),
            OR=if_else(row_number()==2,style_ratio(ORi),str_ORi))
}


OR1_cross <- function(data){
  data %>%
    mutate(N_event =     as.numeric(str_replace(stat_2,"\\([\\d,\\.]+%\\)","")),
           N_nonevent =  as.numeric(str_replace(stat_1,"\\([\\d,\\.]+%\\)",""))) %>%
    mutate(Odds=N_event/N_nonevent,baseOdds=Odds[3]) %>%
    mutate (OR=style_ratio(Odds/baseOdds))
}

OR_smry <- function(data, variable, by, ...){
  rr1 <-  epitools::oddsratio.fisher(data[[by]],data[[variable]])
  stat = str_glue(style_ratio(rr1$measure[[2,1]]),
                  " (", style_ratio(rr1$measure[[2,2]]),",",style_ratio(rr1$measure[[2,3]]),")")
}


OR_cross <- function(data){
  data <- data %>%
    mutate(N_event = as.numeric(str_replace(stat_2,"\\([\\d,\\.]+%\\)","")),
           N_nonevent =  as.numeric(str_replace(stat_1,"\\([\\d,\\.]+%\\)","")),
           N_total=N_event + N_nonevent)
  data %>% mutate(conf_low=OR_conf_low(N_nonevent[2],N_event[2],N_nonevent,N_event),
                  conf_high=OR_conf_high(N_nonevent[2],N_event[2],N_nonevent,N_event)) %>%
    mutate(Odds=N_event/N_nonevent,baseOdds=Odds[3]) %>%
    mutate (ORi=Odds/baseOdds,
            str_ORi=str_c(style_ratio(ORi),"(",style_ratio(conf_low),"-",style_ratio(conf_high),")"),
            OR=if_else(row_number()==2,style_ratio(ORi),str_ORi))
}


OR_conf_low <- function(a,b,c,d){
  t <- NULL
  for(i in 1:length(c)){
    x <- c(a,b, c[i], d[i])
    if (is.na(c[i])) {t1 <- NA }
    else
    {t1 <- fisher.test(matrix(x,nrow=2))%>% broom::tidy() %>% pull(conf.low)}
    t <- c(t,t1)
  }
  return(t)
}


OR_conf_high <- function(a,b,c,d){
  t <- NULL
  for(i in 1:length(c)){
    x <- c(a,b, c[i], d[i])
    if (is.na(c[i])) {t1 <- NA }
    else
    {t1 <- fisher.test(matrix(x,nrow=2))%>% broom::tidy() %>% pull(conf.high)}
    t <- c(t,t1)
  }
  return(t)
}



diff_smry <- function(data, variable, by, ...){
  diff1 <-  t.test(data[[variable]] ~ as.factor(data[[by]]))
  str_glue(style_number(diff1$estimate[2]-diff1$estimate[1]),
           " (", style_ratio(diff1$conf.int[1]),",",style_ratio(diff1$conf.int[2]),")")

}

anovai <-
  function(mean,sd,obs){
    gmean<-sum(mean*obs)/sum(obs)

    df1<-(length(mean)-1)
    df2<-(sum(obs)-length(mean))
    ssbetween<-sum((mean-gmean)^2*obs)
    msbetween<-ssbetween/df1
    sswithin<-sum(sd^2*(obs-1))
    mswithin<-sswithin/df2

    F<-msbetween/mswithin
    p.value<- 1- pf(q=F, df1=df1, df2=df2,lower.tail=T)
    ANOVA<-data.frame(
      df=c(df1,df2),
      Sum_Sq = c(ssbetween, sswithin),
      Mean_Sq = c(msbetween, mswithin),
      F_value=c(F,NA),
      p_value=c(p.value,NA)
    )
    rownames(ANOVA) <- c("Between","Within")
    colnames(ANOVA)<-c("Df","Sum Sq","Mean Sq","F value","Pr(>F)")
    return(ANOVA)
  }




# Hàm tdic: Tạo bảng từ điển dữ liệu với tùy chọn lọc và định dạng
tudien <- function(df, varnames, report = "all", theme = NULL, lang = "vi") {
  # 1. Định nghĩa tiêu đề và tên cột theo ngôn ngữ
  if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr", dependencies = TRUE)
  if (!requireNamespace("gt", quietly = TRUE)) install.packages("gt", dependencies = TRUE)
  if (!requireNamespace("haven", quietly = TRUE)) install.packages("haven", dependencies = TRUE)

  library(dplyr)
  library(gt)
  library(haven)

  if (lang == "vi") {
    header1 <- "Bảng từ điển dữ liệu"
    title1 <- "Tên biến"
    title3 <- "Giá trị biến"
    title4 <- "Nhãn biến"
  } else {
    header1 <- "Data dictionary"
    title1 <- "Variable name"
    title3 <- "Variable values"
    title4 <- "Variable label"
  }

  # 2. Tạo bảng trống để chứa kết quả
  result_table <- tibble(
    Variable = character(),   # Tên biến hoặc nhãn biến
    Value = character(),      # Giá trị biến (phạm vi hoặc danh sách giá trị)
    Label = character(),      # Tên biến gốc
    Type = character()        # Loại biến (định lượng, nhị giá, danh định)
  )

  # 3. Lặp qua từng biến trong dataframe
  for (varname in varnames) {
    var_data <- df[[varname]]                    # Truy xuất cột dữ liệu theo tên biến
    var_label <- attr(var_data, "label", exact = TRUE) %>%
      ifelse(is.null(.), varname, .)  # Lấy nhãn biến nếu có, ngược lại dùng tên biến
    var_values <- attr(var_data, "labels")       # Lấy danh sách giá trị và nhãn (nếu có)

    # 4. Xác định loại biến và định dạng giá trị
    if (!is.null(var_values)) {
      # Biến định tính (có nhãn labels)
      if (length(var_values) == 2) {
        value_str <- paste("Biến nhị giá [",
                           paste(var_values, "=", names(var_values), collapse = "; "), "]")
        type <- "Biến nhị giá"
      } else {
        value_str <- paste("Biến danh định [",
                           paste(var_values, "=", names(var_values), collapse = "; "), "]")
        type <- "Biến danh định"
      }
    } else if (is.numeric(var_data)) {
      # Biến định lượng (nếu không có nhãn labels)
      min_value <- round(min(var_data, na.rm = TRUE), 2)
      max_value <- round(max(var_data, na.rm = TRUE), 2)
      value_str <- paste("Biến định lượng, Phạm vi số liệu [", min_value, "-", max_value, "]")
      type <- "Biến định lượng"
    } else {
      # Biến text không có nhãn
      value_str <- "Text"
      type <- "Biến định tính"
    }

    # Thêm kết quả vào bảng
    result_table <- bind_rows(result_table,
                              tibble(
                                Variable = var_label,
                                Value = value_str,
                                Label = varname,
                                Type = type
                              )
    )
  }

  # 5. Tính tổng số biến
  summary_row <- tibble(
    Variable = "Tổng số biến",
    Value = paste0(
      "Biến định lượng = ", sum(result_table$Type == "Biến định lượng"), "; ",
      "Biến nhị giá = ", sum(result_table$Type == "Biến nhị giá"), "; ",
      "Biến danh định = ", sum(result_table$Type == "Biến danh định")
    ),
    Label = "",
    Type = "Summary"
  )

  # 6. Lọc bảng theo tùy chọn 'report'
  if (report == "Biến định lượng") {
    result_table <- filter(result_table, Type == "Biến định lượng")
  } else if (report == "Biến định tính") {
    result_table <- filter(result_table, Type %in% c("Biến nhị giá", "Biến danh định"))
  }

  # 7. Tùy chọn theme để in đậm
  if (theme == 3) {
    result_table <- summary_row  # Chỉ hiển thị dòng tổng kết khi theme = 3
    highlight_rows <- FALSE
  } else {
    result_table <- bind_rows(result_table, summary_row)  # Thêm dòng tổng kết vào cuối bảng
    if (theme == 1) {
      highlight_rows <- result_table$Type == "Biến định lượng"  # In đậm biến định lượng
    } else if (theme == 2) {
      highlight_rows <- result_table$Type %in% c("Biến nhị giá", "Biến danh định")  # In đậm biến định tính
    } else {
      highlight_rows <- FALSE  # Không in đậm nếu theme không được chọn
    }
  }

  # 8. Hiển thị bảng kết quả với gt
  result_table %>%
    select(-Type) %>%  # Loại bỏ cột 'Type' không cần hiển thị
    gt() %>%
    tab_header(title = header1) %>%
    cols_width(
      Variable ~ px(200),  # Độ rộng cột 'Variable'
      Value ~ px(400),     # Độ rộng cột 'Value'
      Label ~ px(150)      # Độ rộng cột 'Label'
    ) %>%
    tab_style(
      style = list(cell_text(weight = "bold", color = "blue")),
      locations = cells_body(rows = highlight_rows)
    ) %>%
    tab_style(
      style = list(cell_text(weight = "bold")),
      locations = cells_column_labels(columns = everything())
    ) %>%
    tab_options(
      table.width = pct(100)  # Đảm bảo bảng chiếm toàn bộ chiều rộng
    )
}

# Main function for statistical analysis and report generation
rasengan_html <- function(data, vars, by, ib = 1, ratio = "OR", per = "row", p = NULL,
                          output = NULL, digit = 1, autoopen = FALSE, title = NULL,
                          pnote = FALSE, lang = "vie", show_n = FALSE) {

  # Input validation
  lang <- tolower(lang)
  if (!lang %in% c("vie", "eng")) {
    stop("lang must be 'vie' or 'eng'")
  }

  ratio <- toupper(ratio)
  if (!ratio %in% c("OR", "RR", "PR")) {
    stop("ratio must be 'OR', 'RR', or 'PR'")
  }

  per <- tolower(per)
  if (!per %in% c("row", "col")) {
    stop("per must be 'row' or 'col'")
  }

  # Set language-specific labels
  if (lang == "vie") {
    ratio_header <- paste(ratio, "(KTC 95%)")
    if (is.null(title)) title <- "KẾT QUẢ PHÂN TÍCH"
    char_header <- "ĐẶC ĐIỂM"
    mean_label <- "Trung bình ± ĐLC"
    timestamp_label <- "Được tạo lúc:"
    footnote_a <- "Kiểm định chi bình phương"
    footnote_b <- "Kiểm định Fisher"
    ref_label <- "1"
  } else {
    ratio_header <- paste(ratio, "(95% CI)")
    if (is.null(title)) title <- "ANALYSIS RESULTS"
    char_header <- "CHARACTERISTICS"
    mean_label <- "Mean ± SD"
    timestamp_label <- "Generated at:"
    footnote_a <- "Chi-square test"
    footnote_b <- "Fisher test"
    ref_label <- "ref"
  }

  # Handle output file
  if (is.null(output)) {
    file_prefix <- "rasengan"
    counter <- 1
    output <- paste0(file_prefix, ".html")
    while (file.exists(output)) {
      output <- paste0(file_prefix, "_", counter, ".html")
      counter <- counter + 1
      if (counter > 999) stop("Too many files")
    }
  } else if (!grepl("\\.html$", output)) {
    output <- paste0(output, ".html")
  }

  # Create HTML content
  html_content <- c()

  # Add HTML header
  html_content <- c(html_content, '<!DOCTYPE html>')
  html_content <- c(html_content, '<html>')
  html_content <- c(html_content, '<head>')
  html_content <- c(html_content, '<meta charset="UTF-8">')
  html_content <- c(html_content, '<meta name="viewport" content="width=device-width, initial-scale=1.0">')
  html_content <- c(html_content, sprintf('<title>%s</title>', title))

  # Add required scripts
  html_content <- c(html_content, '<script src="https://cdnjs.cloudflare.com/ajax/libs/xlsx/0.18.5/xlsx.full.min.js"></script>')

  # Add CSS styles
  html_content <- c(html_content, '<style>')
  html_content <- c(html_content, '
    body { font-family: Arial, sans-serif; margin: 20px; line-height: 1.6; }
    .container { max-width: 1200px; margin: 0 auto; padding: 20px; }
    table { width: 100%; border-collapse: collapse; margin: 20px 0; }
    th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
    th { background-color: #f8f9fa; font-weight: bold; text-align: center; }
    .center { text-align: center; }
    .right { text-align: right; }
    .group-header { font-weight: bold; background-color: #f8f9fa; }
    .indent { padding-left: 20px; }
    .button-container { margin-bottom: 20px; }
    .btn {
        padding: 10px 20px;
        margin: 5px;
        border: none;
        border-radius: 4px;
        cursor: pointer;
        color: white;
        font-size: 14px;
        transition: opacity 0.3s;
    }
    .btn:hover { opacity: 0.8; }
    .btn-copy { background-color: #005FCC; }
    .btn-word { background-color: #2B579A; }
    .btn-excel { background-color: #217346; }
    .footer { margin-top: 20px; font-size: 12px; color: #666; }
  ')
  html_content <- c(html_content, '</style>')

  # Add JavaScript functions
  html_content <- c(html_content, '<script>')
  html_content <- c(html_content, '
    function copyTable() {
        const table = document.querySelector("table");
        const range = document.createRange();
        range.selectNode(table);
        window.getSelection().removeAllRanges();
        window.getSelection().addRange(range);

        try {
            document.execCommand("copy");
            alert("Table copied to clipboard!");
        } catch (err) {
            console.error("Failed to copy table:", err);
            alert("Failed to copy table");
        }

        window.getSelection().removeAllRanges();
    }

    function exportToWord() {
        const tableHTML = document.querySelector("table").outerHTML;
        const html = `
            <html>
                <head>
                    <meta charset="utf-8">
                    <style>
                        table { border-collapse: collapse; width: 100%; }
                        th, td { border: 1px solid black; padding: 8px; }
                        .center { text-align: center; }
                        .group-header { background-color: #f0f0f0; }
                    </style>
                </head>
                <body>
                    ${tableHTML}
                </body>
            </html>
        `;

        const blob = new Blob([html], { type: "application/msword" });
        const url = URL.createObjectURL(blob);
        const link = document.createElement("a");
        link.href = url;
        link.download = "analysis_report.doc";
        document.body.appendChild(link);
        link.click();
        document.body.removeChild(link);
        URL.revokeObjectURL(url);
    }

    function exportToExcel() {
        const table = document.querySelector("table");
        const wb = XLSX.utils.book_new();
        const ws = XLSX.utils.table_to_sheet(table);
        XLSX.utils.book_append_sheet(wb, ws, "Analysis");
        XLSX.writeFile(wb, "analysis_report.xlsx");
    }
  ')
  html_content <- c(html_content, '</script>')
  html_content <- c(html_content, '</head>')
  html_content <- c(html_content, '<body>')
  html_content <- c(html_content, '<div class="container">')

  # Add title and buttons
  html_content <- c(html_content, sprintf('<h1>%s</h1>', title))
  html_content <- c(html_content, '
    <div class="button-container">
        <button class="btn btn-copy" onclick="copyTable()">Copy Table</button>
        <button class="btn btn-word" onclick="exportToWord()">Export to Word</button>
        <button class="btn btn-excel" onclick="exportToExcel()">Export to Excel</button>
    </div>
  ')

  # Start table
  html_content <- c(html_content, '<table>')

  # Add table headers
  by_label <- names(data)[which(names(data) == by)]
  by_levels <- sort(unique(data[[by]]))

  html_content <- c(html_content, sprintf('
    <tr>
        <th rowspan="2" style="text-align: left;">%s</th>
        <th colspan="2">%s</th>
        <th rowspan="2">p</th>
        <th rowspan="2">%s</th>
    </tr>
  ', char_header, by_label, ratio_header))

  html_content <- c(html_content, sprintf('
    <tr>
        <th>%s (n %%)</th>
        <th>%s (n %%)</th>
    </tr>
  ', by_levels[2], by_levels[1]))

  # Process each variable
  for (var in vars) {
    var_label <- var
    if (show_n) {
      n_total <- sum(!is.na(data[[var]]))
      var_label <- sprintf("%s (N=%d)", var_label, n_total)
    }

    # Add variable header
    html_content <- c(html_content, sprintf('
      <tr>
          <td class="group-header" colspan="5">%s</td>
      </tr>
    ', var_label))

    # Check if numeric
    if (is.numeric(data[[var]])) {
      # Check if categorical
      unique_vals <- unique(na.omit(data[[var]]))
      is_categorical <- length(unique_vals) <= 10

      if (!is_categorical) {
        # Continuous variable analysis
        stats <- by(data[[var]], data[[by]], function(x) {
          c(mean = mean(x, na.rm = TRUE),
            sd = sd(x, na.rm = TRUE))
        })

        t_test <- t.test(data[[var]] ~ data[[by]])
        p_val <- format(t_test$p.value, digits = 3)

        if (pnote) p_val <- sprintf('%s<sup>b</sup>', p_val)

        html_content <- c(html_content, sprintf('
          <tr>
              <td class="indent">%s</td>
              <td class="center">%.1f ± %.1f</td>
              <td class="center">%.1f ± %.1f</td>
              <td class="center">%s</td>
              <td class="center">-</td>
          </tr>
        ', mean_label,
                                                stats[[2]]["mean"], stats[[2]]["sd"],
                                                stats[[1]]["mean"], stats[[1]]["sd"],
                                                p_val))

        next
      }
    }

    # Categorical analysis
    levels <- sort(unique(na.omit(data[[var]])))
    ref_level <- levels[ib]

    for (l in levels) {
      n1 <- sum(data[[var]] == l & data[[by]] == 1, na.rm = TRUE)
      n0 <- sum(data[[var]] == l & data[[by]] == 0, na.rm = TRUE)
      total1 <- sum(data[[by]] == 1, na.rm = TRUE)
      total0 <- sum(data[[by]] == 0, na.rm = TRUE)

      if (per == "row") {
        total_row <- n1 + n0
        pct1 <- sprintf("%.1f", 100 * n1/total_row)
        pct0 <- sprintf("%.1f", 100 * n0/total_row)
      } else {
        pct1 <- sprintf("%.1f", 100 * n1/total1)
        pct0 <- sprintf("%.1f", 100 * n0/total0)
      }

      if (l == ref_level) {
        ratio_display <- ref_label
        p_display <- ""
      } else {
        # Calculate ratio and p-value
        temp_data <- data.frame(
          y = data[[by]],
          x = as.numeric(data[[var]] == l)
        )

        tryCatch({
          if (ratio == "OR") {
            mod <- glm(y ~ x, family = binomial(), data = temp_data)
          } else if (ratio %in% c("RR", "PR")) {
            mod <- glm(y ~ x, family = poisson(), data = temp_data)
          }

          coef <- summary(mod)$coefficients
          est <- exp(coef[2, 1])
          ci <- exp(coef[2, 1] + c(-1, 1) * 1.96 * coef[2, 2])

          ratio_display <- if (est == 1) {
            "(empty)"
          } else {
            sprintf("%.2f (%.2f-%.2f)", est, ci[1], ci[2])
          }

          p_val <- format(coef[2, 4], digits = 3)
          p_display <- if (pnote) sprintf('%s<sup>a</sup>', p_val) else p_val

        }, error = function(e) {
          ratio_display <<- ""
          p_display <<- ""
        })
      }

      html_content <- c(html_content, sprintf('
        <tr>
            <td class="indent">%s</td>
            <td class="center">%d (%.1f%%)</td>
            <td class="center">%d (%.1f%%)</td>
            <td class="center">%s</td>
            <td class="center">%s</td>
        </tr>
      ', l, n1, as.numeric(pct1), n0, as.numeric(pct0), p_display, ratio_display))
    }
  }

  # Close table
  html_content <- c(html_content, '</table>')

  # Add footnotes if needed
  if (pnote) {
    html_content <- c(html_content, sprintf('
      <div class="footer">
          <p>a: %s<br>b: %s</p>
      </div>
    ', footnote_a, footnote_b))
  }

  # Add timestamp
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  html_content <- c(html_content, sprintf('
    <div class="footer">
        <p>%s %s</p>
    </div>
  ', timestamp_label, timestamp))

  # Close HTML
  html_content <- c(html_content, '</div>')
  html_content <- c(html_content, '</body>')
  html_content <- c(html_content, '</html>')

  # Write to file
  writeLines(html_content, output, useBytes = TRUE)

  # Open file if requested
  if (autoopen) {
    if (.Platform$OS.type == "windows") {
      shell.exec(output)
    } else {
      system(paste("open", output))
    }
  }

  # Return values
  return(list(
    output = output,
    timestamp = timestamp
  ))
}


sharinganhtml <- function(data = NULL, varlist = NULL, digit = 1, output = NULL,
                          title = "Báo cáo thống kê mô tả", theme = "light", nosort = FALSE,
                          indent = NULL, missing = FALSE, by = NULL, merge_by = FALSE) {
  # Kiểm tra dữ liệu đầu vào
  if (is.null(data)) {
    # Nếu không cung cấp data, thử lấy từ môi trường cha
    tryCatch({
      df <- get(".", envir = parent.frame(), inherits = FALSE)
    }, error = function(e) {
      stop("Không tìm thấy dữ liệu. Vui lòng cung cấp tham số data.")
    })
  } else if (is.data.frame(data)) {
    # Nếu data là dataframe, sử dụng nó
    df <- data
  } else {
    stop("Tham số data phải là một data frame")
  }

  # Kiểm tra nếu digit hợp lệ
  if (digit < 0) {
    stop("Digit option must be non-negative")
  }

  # Thiết lập màu sắc dựa trên theme
  bg_color <- "#ffffff"
  text_color <- "#000000"
  border_color <- "#dddddd"
  header_bg <- "#f2f2f2"
  row_alt_bg <- "#f9f9f9"
  indent_style <- ""

  if (theme == "dark") {
    bg_color <- "#2d2d2d"
    text_color <- "#ffffff"
    border_color <- "#555555"
    header_bg <- "#3d3d3d"
    row_alt_bg <- "#353535"
  }

  if (!is.null(indent)) {
    indent_style <- sprintf(" style='padding-left: %spx;'", indent)
  }

  # Nếu varlist là NULL, sử dụng tất cả biến trong dataframe (trừ biến by nếu có)
  if (is.null(varlist)) {
    varlist <- setdiff(names(df), by)
  } else if (is.character(varlist)) {
    # Kiểm tra xem tất cả biến trong varlist có tồn tại trong dataframe không
    if (!all(varlist %in% names(df))) {
      stop("Some variables in varlist do not exist in the data frame")
    }
  } else {
    stop("varlist must be a character vector")
  }

  # Kiểm tra tham số by
  has_by <- !is.null(by)
  if (has_by) {
    if (!is.character(by)) {
      stop("by must be a character vector")
    }
    if (!all(by %in% names(df))) {
      stop("Some variables in by do not exist in the data frame")
    }
    # Đảm bảo biến by không nằm trong varlist
    varlist <- setdiff(varlist, by)
  }

  # Biến với giá trị missing
  vars_with_missing <- character(0)
  total_cells <- 0
  total_missing <- 0

  # Kiểm tra giá trị missing nếu tùy chọn missing được chỉ định
  if (missing) {
    total_obs <- nrow(df)

    for (var in varlist) {
      total_cells <- total_cells + total_obs
      curr_missing <- sum(is.na(df[[var]]))
      total_missing <- total_missing + curr_missing

      if (curr_missing > 0) {
        vars_with_missing <- c(vars_with_missing, var)
      }
    }

    # Nếu tùy chọn missing được chỉ định, chỉ sử dụng các biến có giá trị missing
    if (length(vars_with_missing) > 0) {
      varlist <- vars_with_missing
    }
  }

  # Xử lý tên file đầu ra
  file_prefix <- "sharingan"
  if (is.null(output)) {
    output <- paste0(file_prefix, ".html")
  }

  counter <- 1
  while (file.exists(output)) {
    output <- paste0(file_prefix, "_", counter, ".html")
    counter <- counter + 1
    if (counter > 999) {
      stop("Đã tồn tại quá nhiều tệp")
    }
  }

  # Tạo nội dung HTML
  html <- c(
    "<!DOCTYPE html>",
    "<html><head>",
    "<meta charset='UTF-8'>",
    "<meta name='viewport' content='width=device-width, initial-scale=1.0'>",
    paste0("<title>", title, "</title>"),
    "<style>",
    paste0("body {font-family: Arial, sans-serif; margin: 20px; background: ", bg_color, "; color: ", text_color, ";}"),
    "h1 {text-align: center;}",
    ".container {max-width: 1200px; margin: 0 auto; position: relative;}",
    ".export-buttons {text-align: center; margin-bottom: 20px;}",
    ".btn {",
    "    background-color: #4CAF50;",
    "    border: none;",
    "    color: white;",
    "    padding: 10px 20px;",
    "    text-align: center;",
    "    text-decoration: none;",
    "    display: inline-block;",
    "    font-size: 16px;",
    "    margin: 4px 2px;",
    "    cursor: pointer;",
    "    border-radius: 4px;",
    "}",
    ".copy-btn {background-color: #4CAF50;}",
    ".word-btn {background-color: #2b579a;}",
    ".excel-btn {background-color: #217346;}",
    ".decimal-btn {background-color: #ff9800;}",
    ".btn:hover {opacity: 0.8;}",
    paste0("table {width: 100%; border-collapse: collapse; margin: 20px 0;}"),
    paste0("th, td {border: 1px solid ", border_color, "; padding: 12px; text-align: left;}"),
    paste0("th {background: ", header_bg, "; font-weight: bold; text-align: center;}"),
    paste0("tr:nth-child(even) {background: ", row_alt_bg, ";}"),
    paste0(".var-header {background: ", header_bg, "; font-weight: bold;}"),
    ".continuous-stats {font-style: italic;}",
    ".number {text-align: center;}",
    ".center {text-align: center;}",
    ".summary-text {text-align: center; font-weight: bold; margin-top: 20px;}",
    ".notification {position: fixed; top: 20px; right: 20px; background-color: #4CAF50;",
    "  color: white; padding: 16px; border-radius: 4px; display: none;}",
    ".toggle-container {display: inline-block; position: relative; margin-left: 10px;}",
    ".toggle-label {font-size: 14px; margin-right: 5px;}",
    ".group-header {background-color: #4b6584; color: white; text-align: center; font-weight: bold;}",
    ".by-section {margin-top: 40px; border-top: 2px solid #ccc; padding-top: 20px;}",
    ".by-title {font-size: 20px; font-weight: bold; margin-bottom: 10px; color: #4b6584;}",
    "@media print {",
    "  body {background: white; color: black;}",
    "  table {border: 1px solid black;}",
    "  th, td {border: 1px solid black;}",
    "  .export-buttons, .notification {display: none;}",
    "}",
    "@media (max-width: 600px) {",
    "  table {font-size: 14px;}",
    "  th, td {padding: 8px;}",
    "}",
    "</style>",

    "<!-- Thêm thư viện FileSaver.js để xuất file -->",
    "<script src='https://cdnjs.cloudflare.com/ajax/libs/FileSaver.js/2.0.5/FileSaver.min.js'></script>",

    "<!-- Thêm thư viện SheetJS để xuất Excel -->",
    "<script src='https://cdnjs.cloudflare.com/ajax/libs/xlsx/0.18.5/xlsx.full.min.js'></script>",

    "<!-- Thêm thư viện docx để xuất Word -->",
    "<script src='https://cdnjs.cloudflare.com/ajax/libs/docxtemplater/3.37.11/docxtemplater.js'></script>",
    "<script src='https://cdnjs.cloudflare.com/ajax/libs/pizzip/3.1.4/pizzip.min.js'></script>",
    "<script src='https://cdnjs.cloudflare.com/ajax/libs/pizzip/3.1.4/pizzip-utils.min.js'></script>",

    "<script>",
    "function showNotification(message) {",
    "    const notification = document.getElementById('notification');",
    "    notification.textContent = message;",
    "    notification.style.display = 'block';",
    "    setTimeout(() => {",
    "        notification.style.display = 'none';",
    "    }, 2000);",
    "}",

    "function copyTableToClipboard() {",
    "    const tables = document.querySelectorAll('table');",
    "    let combinedTables = '';",
    "    tables.forEach(table => {",
    "        combinedTables += table.outerHTML + '<br><br>';",
    "    });",
    "    ",
    "    // Tạo một element tạm thời để chứa HTML",
    "    const tempDiv = document.createElement('div');",
    "    tempDiv.innerHTML = combinedTables;",
    "    document.body.appendChild(tempDiv);",
    "    ",
    "    const range = document.createRange();",
    "    range.selectNode(tempDiv);",
    "    window.getSelection().removeAllRanges();",
    "    window.getSelection().addRange(range);",
    "    try {",
    "        document.execCommand('copy');",
    "        showNotification('Bảng đã được sao chép vào clipboard!');",
    "    } catch (err) {",
    "        console.error('Failed to copy table:', err);",
    "    }",
    "    window.getSelection().removeAllRanges();",
    "    document.body.removeChild(tempDiv);",
    "}",

    "function exportToExcel() {",
    "    try {",
    "        // Lấy tiêu đề",
    "        const title = document.querySelector('h1').textContent;",
    "        ",
    "        // Tạo workbook mới",
    "        const wb = XLSX.utils.book_new();",
    "        ",
    "        // Lấy tất cả các bảng và thêm vào workbook",
    "        const tables = document.querySelectorAll('table');",
    "        tables.forEach((table, index) => {",
    "            const ws = XLSX.utils.table_to_sheet(table);",
    "            const sheetName = 'Sheet' + (index + 1);",
    "            XLSX.utils.book_append_sheet(wb, ws, sheetName);",
    "        });",
    "        ",
    "        // Xuất file Excel",
    "        XLSX.writeFile(wb, title + '.xlsx');",
    "        showNotification('Xuất Excel thành công!');",
    "    } catch (error) {",
    "        console.error('Lỗi khi xuất Excel:', error);",
    "        showNotification('Lỗi khi xuất Excel!');",
    "    }",
    "}",

    "function exportToWord() {",
    "    try {",
    "        // Lấy nội dung HTML",
    "        const title = document.querySelector('h1').textContent;",
    "        let allTables = '';",
    "        const tables = document.querySelectorAll('table');",
    "        const byTitles = document.querySelectorAll('.by-title');",
    "        ",
    "        // Kết hợp tất cả các bảng và tiêu đề phân nhóm",
    "        let tableIndex = 0;",
    "        byTitles.forEach((byTitle, index) => {",
    "            allTables += '<h2>' + byTitle.textContent + '</h2>';",
    "            if (tableIndex < tables.length) {",
    "                allTables += tables[tableIndex].outerHTML + '<br><br>';",
    "                tableIndex++;",
    "            }",
    "        });",
    "        ",
    "        // Thêm bảng không phân nhóm nếu còn",
    "        if (byTitles.length === 0 && tables.length > 0) {",
    "            allTables += tables[0].outerHTML;",
    "        }",
    "        ",
    "        const timestamp = document.querySelector('p small').textContent;",
    "        ",
    "        // Tạo nội dung Word",
    "        const content = `",
    "            <html xmlns:o='urn:schemas-microsoft-com:office:office' ",
    "                  xmlns:w='urn:schemas-microsoft-com:office:word' ",
    "                  xmlns='http://www.w3.org/TR/REC-html40'>",
    "            <head>",
    "                <meta charset='utf-8'>",
    "                <title>${title}</title>",
    "                <style>",
    "                    table {border-collapse: collapse; width: 100%; margin-bottom: 20px;}",
    "                    th, td {border: 1px solid black; padding: 8px;}",
    "                    th {background-color: #f2f2f2; font-weight: bold;}",
    "                    .var-header {background-color: #f2f2f2; font-weight: bold;}",
    "                    .continuous-stats {font-style: italic;}",
    "                    h1 {text-align: center;}",
    "                    h2 {color: #4b6584; margin-top: 30px;}",
    "                    .group-header {background-color: #4b6584; color: white;}",
    "                </style>",
    "            </head>",
    "            <body>",
    "                <h1>${title}</h1>",
    "                ${allTables}",
    "                <p><small>${timestamp}</small></p>",
    "            </body>",
    "            </html>",
    "        `;",
    "        ",
    "        // Chuyển đổi sang định dạng Word",
    "        const blob = new Blob([content], {",
    "            type: 'application/vnd.ms-word;charset=utf-8'",
    "        });",
    "        ",
    "        // Tải xuống file",
    "        saveAs(blob, title + '.doc');",
    "        showNotification('Xuất Word thành công!');",
    "    } catch (error) {",
    "        console.error('Lỗi khi xuất Word:', error);",
    "        showNotification('Lỗi khi xuất Word!');",
    "    }",
    "}",

    "// Hàm chuyển đổi dấu thập phân",
    "function toggleDecimalFormat() {",
    "    const btn = document.getElementById('decimal-btn');",
    "    const useComma = btn.getAttribute('data-comma') === 'true';",
    "    ",
    "    // Lấy tất cả các phần tử chứa số thập phân",
    "    const numericElements = document.querySelectorAll('.number, .center');",
    "    ",
    "    numericElements.forEach(el => {",
    "        let text = el.textContent;",
    "        ",
    "        // Không xử lý các ô chỉ chứa số nguyên hoặc không có số",
    "        if (!text.match(/[0-9][-+]?[.,][0-9]/)) return;",
    "        ",
    "        if (useComma) {",
    "            // Chuyển từ dấu phẩy sang dấu chấm",
    "            text = text.replace(/([0-9]),([0-9])/g, '$1.$2');",
    "        } else {",
    "            // Chuyển từ dấu chấm sang dấu phẩy",
    "            text = text.replace(/([0-9])\\.([0-9])/g, '$1,$2');",
    "        }",
    "        ",
    "        el.textContent = text;",
    "    });",
    "    ",
    "    // Cập nhật trạng thái nút và thông báo",
    "    btn.setAttribute('data-comma', !useComma);",
    "    const newFormat = !useComma ? 'dấu phẩy (,)' : 'dấu chấm (.)';",
    "    btn.textContent = 'Chuyển sang ' + newFormat;",
    "    showNotification('Đã chuyển sang sử dụng ' + newFormat);",
    "    ",
    "    // Cập nhật thông tin dấu thập phân ở cuối trang",
    "    const decimalInfoEl = document.getElementById('decimal-info');",
    "    decimalInfoEl.textContent = 'Dấu thập phân: ' + (useComma ? 'dấu chấm (.)' : 'dấu phẩy (,)');",
    "}",

    "// Hàm khởi tạo khi tải trang",
    "window.onload = function() {",
    "    // Khởi tạo nút chuyển đổi dấu thập phân",
    "    const decimalBtn = document.getElementById('decimal-btn');",
    "    decimalBtn.setAttribute('data-comma', 'false');",
    "}",
    "</script>",
    "</head><body>",
    paste0("<h1>", title, "</h1>"),
    "<div class='container'>",
    "<div class='export-buttons'>",
    "<button class='btn copy-btn' onclick='copyTableToClipboard()'>Sao chép bảng</button>",
    "<button class='btn excel-btn' onclick='exportToExcel()'>Xuất Excel</button>",
    "<button class='btn word-btn' onclick='exportToWord()'>Xuất Word</button>",
    "<button id='decimal-btn' class='btn decimal-btn' onclick='toggleDecimalFormat()' data-comma='false'>Chuyển sang dấu phẩy (,)</button>",
    "</div>",
    "<div id='notification' class='notification'></div>"
  )

  # Hàm định dạng số với số chữ số thập phân đã chỉ định
  format_number <- function(x, digits = digit) {
    if (is.na(x)) return("NA")
    return(sprintf(paste0("%.", digits, "f"), x))
  }

  # Hàm tạo bảng phân tích cho một biến by
  create_by_table <- function(by_var, by_values, by_labels = NULL) {
    # Tạo HTML cho bảng
    table_html <- c("<table>", "<tr><th rowspan='2'>Đặc điểm</th>")

    # Thêm tiêu đề cột cho mỗi giá trị của biến by
    for (by_val in by_values) {
      # Lấy nhãn của giá trị by nếu có
      if (!is.null(by_labels)) {
        label_idx <- which(by_labels == by_val)
        by_display <- if (length(label_idx) > 0) names(by_labels)[label_idx[1]] else by_val
      } else {
        by_display <- by_val
      }

      table_html <- c(table_html, paste0("<th colspan='2' class='group-header'>", by_display, "</th>"))
    }
    table_html <- c(table_html, "</tr>")

    # Hàng thứ hai với tiêu đề Tần số, Tỷ lệ cho mỗi nhóm
    table_html <- c(table_html, "<tr>")
    for (i in 1:length(by_values)) {
      table_html <- c(table_html, "<th class='number'>Tần số</th><th class='number'>Tỷ lệ (%)</th>")
    }
    table_html <- c(table_html, "</tr>")

    # Xử lý từng biến phân tích
    for (var in varlist) {
      tryCatch({
        # Lấy nhãn biến hoặc sử dụng tên biến nếu không có nhãn
        var_label <- attr(df[[var]], "label")
        if (is.null(var_label)) var_label <- var

        # Thêm hàng tiêu đề biến
        table_html <- c(table_html, paste0("<tr><td class='var-header'", indent_style, ">", var_label, "</td>"))
        # Thêm ô trống cho mỗi nhóm
        for (i in 1:length(by_values)) {
          table_html <- c(table_html, "<td colspan='2'></td>")
        }
        table_html <- c(table_html, "</tr>")

        # Kiểm tra xem biến là liên tục hay phân loại
        is_continuous <- is.numeric(df[[var]]) && length(unique(na.omit(df[[var]]))) >= 10

        if (is_continuous) {
          # Xử lý biến liên tục
          # Hàng trung bình ± SD
          table_html <- c(table_html, paste0("<tr><td class='continuous-stats'", indent_style, ">Trung bình ± SD</td>"))

          for (by_val in by_values) {
            subset_data <- df[df[[by_var]] == by_val, ]
            mean_val <- mean(subset_data[[var]], na.rm = TRUE)
            sd_val <- sd(subset_data[[var]], na.rm = TRUE)

            mean_str <- format_number(mean_val)
            sd_str <- format_number(sd_val)

            table_html <- c(table_html, paste0("<td class='center' colspan='2'>", mean_str, " ± ", sd_str, "</td>"))
          }

          table_html <- c(table_html, "</tr>")

          # Hàng trung vị (IQR)
          table_html <- c(table_html, paste0("<tr><td class='continuous-stats'", indent_style, ">Trung vị (IQR)</td>"))

          for (by_val in by_values) {
            subset_data <- df[df[[by_var]] == by_val, ]
            median_val <- median(subset_data[[var]], na.rm = TRUE)
            p25 <- quantile(subset_data[[var]], 0.25, na.rm = TRUE)
            p75 <- quantile(subset_data[[var]], 0.75, na.rm = TRUE)

            median_str <- format_number(median_val)
            p25_str <- format_number(p25)
            p75_str <- format_number(p75)

            table_html <- c(table_html, paste0("<td class='center' colspan='2'>", median_str, " (", p25_str, " - ", p75_str, ")</td>"))
          }

          table_html <- c(table_html, "</tr>")

          # Thêm missing nếu có
          missing_counts <- sapply(by_values, function(by_val) {
            subset_data <- df[df[[by_var]] == by_val, ]
            return(sum(is.na(subset_data[[var]])))
          })

          if (any(missing_counts > 0)) {
            table_html <- c(table_html, paste0("<tr><td class='continuous-stats'", indent_style, ">Missing</td>"))

            for (i in seq_along(by_values)) {
              by_val <- by_values[i]
              subset_data <- df[df[[by_var]] == by_val, ]
              miss_count <- missing_counts[i]
              total_count <- nrow(subset_data)
              miss_percent <- 100 * miss_count / total_count
              miss_percent_str <- format_number(miss_percent)

              table_html <- c(table_html, paste0("<td class='number'>", miss_count, "</td><td class='number'>", miss_percent_str, " %</td>"))
            }

            table_html <- c(table_html, "</tr>")
          }
        } else {
          # Xử lý biến phân loại
          all_categories <- unique(na.omit(df[[var]]))

          # Kiểm tra xem biến có nhãn giá trị không
          value_labels <- attr(df[[var]], "labels")

          # Thêm missing vào danh sách danh mục nếu có
          if (any(is.na(df[[var]]))) {
            all_categories <- c(all_categories, NA)
          }

          # Xử lý từng danh mục
          for (cat in all_categories) {
            # Lấy tên hiển thị cho danh mục
            if (is.na(cat)) {
              display_cat <- "Missing"
            } else if (!is.null(value_labels)) {
              # Tìm nhãn cho giá trị này nếu có
              if (is.numeric(cat)) {
                label_idx <- which(value_labels == cat)
                display_cat <- if (length(label_idx) > 0) names(value_labels)[label_idx[1]] else cat
              } else {
                display_cat <- cat
              }
            } else {
              display_cat <- cat
            }

            table_html <- c(table_html, paste0("<tr><td", indent_style, ">", display_cat, "</td>"))

            # Tính tần số và tỷ lệ cho mỗi nhóm
            for (by_val in by_values) {
              subset_data <- df[df[[by_var]] == by_val, ]

              if (is.na(cat)) {
                freq <- sum(is.na(subset_data[[var]]))
              } else {
                freq <- sum(subset_data[[var]] == cat, na.rm = TRUE)
              }

              total <- nrow(subset_data)
              percent <- 100 * freq / total
              percent_str <- format_number(percent)

              table_html <- c(table_html, paste0("<td class='number'>", freq, "</td><td class='number'>", percent_str, " %</td>"))
            }

            table_html <- c(table_html, "</tr>")
          }
        }
      }, error = function(e) {
        warning(paste("Lỗi khi xử lý biến", var, ":", e$message))
      })
    }

    # Đóng bảng
    table_html <- c(table_html, "</table>")
    return(table_html)
  }

  # Xử lý trường hợp phân tầng theo biến (by)
  if (has_by) {
    if (merge_by && length(by) > 1) {
      # Tạo bảng phân tầng cho từng biến by và hợp nhất chúng
      for (i in seq_along(by)) {
        by_var <- by[i]
        by_values <- unique(na.omit(df[[by_var]]))
        by_labels <- attr(df[[by_var]], "labels")

        # Lấy nhãn biến hoặc sử dụng tên biến nếu không có nhãn
        by_var_label <- attr(df[[by_var]], "label")
        if (is.null(by_var_label)) by_var_label <- by_var

        # Thêm tiêu đề phân nhóm
        html <- c(html,
                  paste0("<div class='by-section'>"),
                  paste0("<div class='by-title'>Phân tầng theo ", by_var_label, "</div>"))

        # Tạo bảng phân tầng cho biến by này
        by_table <- create_by_table(by_var, by_values, by_labels)
        html <- c(html, by_table, "</div>")
      }
    } else if (length(by) == 1) {
      # Xử lý trường hợp có 1 biến by - tạo bảng phân tầng đơn
      by_var <- by[1]
      by_values <- unique(na.omit(df[[by_var]]))
      by_labels <- attr(df[[by_var]], "labels")

      # Tạo bảng
      html <- c(html, "<table>")

      # Tạo header cho bảng với các nhóm
      html <- c(html, "<tr><th rowspan='2'>Đặc điểm</th>")

      # Thêm tiêu đề cột cho mỗi giá trị của biến by
      for (by_val in by_values) {
        # Lấy nhãn của giá trị by nếu có
        if (!is.null(by_labels)) {
          label_idx <- which(by_labels == by_val)
          by_display <- if (length(label_idx) > 0) names(by_labels)[label_idx[1]] else by_val
        } else {
          by_display <- by_val
        }

        html <- c(html, paste0("<th colspan='2' class='group-header'>", by_display, "</th>"))
      }
      html <- c(html, "</tr>")

      # Hàng thứ hai với tiêu đề Tần số, Tỷ lệ cho mỗi nhóm
      html <- c(html, "<tr>")
      for (i in 1:length(by_values)) {
        html <- c(html, "<th class='number'>Tần số</th><th class='number'>Tỷ lệ (%)</th>")
      }
      html <- c(html, "</tr>")

      # Xử lý từng biến phân tích
      for (var in varlist) {
        tryCatch({
          # Lấy nhãn biến hoặc sử dụng tên biến nếu không có nhãn
          var_label <- attr(df[[var]], "label")
          if (is.null(var_label)) var_label <- var

          # Thêm hàng tiêu đề biến
          html <- c(html, paste0("<tr><td class='var-header'", indent_style, ">", var_label, "</td>"))
          # Thêm ô trống cho mỗi nhóm
          for (i in 1:length(by_values)) {
            html <- c(html, "<td colspan='2'></td>")
          }
          html <- c(html, "</tr>")

          # Kiểm tra xem biến là liên tục hay phân loại
          is_continuous <- is.numeric(df[[var]]) && length(unique(na.omit(df[[var]]))) >= 10

          if (is_continuous) {
            # Xử lý biến liên tục
            # Hàng trung bình ± SD
            html <- c(html, paste0("<tr><td class='continuous-stats'", indent_style, ">Trung bình ± SD</td>"))

            for (by_val in by_values) {
              subset_data <- df[df[[by_var]] == by_val, ]
              mean_val <- mean(subset_data[[var]], na.rm = TRUE)
              sd_val <- sd(subset_data[[var]], na.rm = TRUE)

              mean_str <- format_number(mean_val)
              sd_str <- format_number(sd_val)

              html <- c(html, paste0("<td class='center' colspan='2'>", mean_str, " ± ", sd_str, "</td>"))
            }

            html <- c(html, "</tr>")

            # Hàng trung vị (IQR)
            html <- c(html, paste0("<tr><td class='continuous-stats'", indent_style, ">Trung vị (IQR)</td>"))

            for (by_val in by_values) {
              subset_data <- df[df[[by_var]] == by_val, ]
              median_val <- median(subset_data[[var]], na.rm = TRUE)
              p25 <- quantile(subset_data[[var]], 0.25, na.rm = TRUE)
              p75 <- quantile(subset_data[[var]], 0.75, na.rm = TRUE)

              median_str <- format_number(median_val)
              p25_str <- format_number(p25)
              p75_str <- format_number(p75)

              html <- c(html, paste0("<td class='center' colspan='2'>", median_str, " (", p25_str, " - ", p75_str, ")</td>"))
            }

            html <- c(html, "</tr>")

            # Thêm missing nếu có
            missing_counts <- sapply(by_values, function(by_val) {
              subset_data <- df[df[[by_var]] == by_val, ]
              return(sum(is.na(subset_data[[var]])))
            })

            if (any(missing_counts > 0)) {
              html <- c(html, paste0("<tr><td class='continuous-stats'", indent_style, ">Missing</td>"))

              for (i in seq_along(by_values)) {
                by_val <- by_values[i]
                subset_data <- df[df[[by_var]] == by_val, ]
                miss_count <- missing_counts[i]
                total_count <- nrow(subset_data)
                miss_percent <- 100 * miss_count / total_count
                miss_percent_str <- format_number(miss_percent)

                html <- c(html, paste0("<td class='number'>", miss_count, "</td><td class='number'>", miss_percent_str, " %</td>"))
              }

              html <- c(html, "</tr>")
            }
          } else {
            # Xử lý biến phân loại
            all_categories <- unique(na.omit(df[[var]]))

            # Kiểm tra xem biến có nhãn giá trị không
            value_labels <- attr(df[[var]], "labels")

            # Thêm missing vào danh sách danh mục nếu có
            if (any(is.na(df[[var]]))) {
              all_categories <- c(all_categories, NA)
            }

            # Xử lý từng danh mục
            for (cat in all_categories) {
              # Lấy tên hiển thị cho danh mục
              if (is.na(cat)) {
                display_cat <- "Missing"
              } else if (!is.null(value_labels)) {
                # Tìm nhãn cho giá trị này nếu có
                if (is.numeric(cat)) {
                  label_idx <- which(value_labels == cat)
                  display_cat <- if (length(label_idx) > 0) names(value_labels)[label_idx[1]] else cat
                } else {
                  display_cat <- cat
                }
              } else {
                display_cat <- cat
              }

              html <- c(html, paste0("<tr><td", indent_style, ">", display_cat, "</td>"))

              # Tính tần số và tỷ lệ cho mỗi nhóm
              for (by_val in by_values) {
                subset_data <- df[df[[by_var]] == by_val, ]

                if (is.na(cat)) {
                  freq <- sum(is.na(subset_data[[var]]))
                } else {
                  freq <- sum(subset_data[[var]] == cat, na.rm = TRUE)
                }

                total <- nrow(subset_data)
                percent <- 100 * freq / total
                percent_str <- format_number(percent)

                html <- c(html, paste0("<td class='number'>", freq, "</td><td class='number'>", percent_str, " %</td>"))
              }

              html <- c(html, "</tr>")
            }
          }
        }, error = function(e) {
          warning(paste("Lỗi khi xử lý biến", var, ":", e$message))
        })
      }
    } else if (length(by) > 1 && !merge_by) {
      # Trường hợp có nhiều biến by (2 biến trở lên) - phân tầng chéo
      # Sử dụng 2 biến đầu tiên để tạo bảng chéo
      by_var1 <- by[1]
      by_var2 <- by[2]

      by_values1 <- unique(na.omit(df[[by_var1]]))
      by_values2 <- unique(na.omit(df[[by_var2]]))

      # Lấy nhãn nếu có
      by_labels1 <- attr(df[[by_var1]], "labels")
      by_labels2 <- attr(df[[by_var2]], "labels")

      # Tạo header cho bảng với các nhóm chéo
      html <- c(html, "<table>")
      html <- c(html, "<tr><th rowspan='2'>Đặc điểm</th>")

      # Header cho biến by thứ nhất
      for (by_val1 in by_values1) {
        # Lấy nhãn của giá trị by nếu có
        if (!is.null(by_labels1)) {
          label_idx <- which(by_labels1 == by_val1)
          by_display1 <- if (length(label_idx) > 0) names(by_labels1)[label_idx[1]] else by_val1
        } else {
          by_display1 <- by_val1
        }

        html <- c(html, paste0("<th colspan='", 2 * length(by_values2), "' class='group-header'>", by_display1, "</th>"))
      }
      html <- c(html, "</tr><tr>")

      # Header cho biến by thứ hai (lặp lại cho mỗi giá trị của biến by thứ nhất)
      for (by_val1 in by_values1) {
        for (by_val2 in by_values2) {
          # Lấy nhãn của giá trị by nếu có
          if (!is.null(by_labels2)) {
            label_idx <- which(by_labels2 == by_val2)
            by_display2 <- if (length(label_idx) > 0) names(by_labels2)[label_idx[1]] else by_val2
          } else {
            by_display2 <- by_val2
          }

          html <- c(html, paste0("<th colspan='2' class='group-header'>", by_display2, "</th>"))
        }
      }
      html <- c(html, "</tr><tr><th></th>")

      # Hàng tiêu đề cuối cùng với Tần số, Tỷ lệ
      for (i in 1:(length(by_values1) * length(by_values2))) {
        html <- c(html, "<th class='number'>Tần số</th><th class='number'>Tỷ lệ (%)</th>")
      }
      html <- c(html, "</tr>")

      # Xử lý từng biến phân tích
      for (var in varlist) {
        tryCatch({
          # Lấy nhãn biến hoặc sử dụng tên biến nếu không có nhãn
          var_label <- attr(df[[var]], "label")
          if (is.null(var_label)) var_label <- var

          # Thêm hàng tiêu đề biến
          html <- c(html, paste0("<tr><td class='var-header'", indent_style, ">", var_label, "</td>"))

          # Thêm ô trống cho mỗi nhóm chéo
          for (i in 1:(length(by_values1) * length(by_values2))) {
            html <- c(html, "<td colspan='2'></td>")
          }
          html <- c(html, "</tr>")

          # Kiểm tra xem biến là liên tục hay phân loại
          is_continuous <- is.numeric(df[[var]]) && length(unique(na.omit(df[[var]]))) >= 10

          if (is_continuous) {
            # Xử lý biến liên tục cho các nhóm chéo
            # Hàng trung bình ± SD
            html <- c(html, paste0("<tr><td class='continuous-stats'", indent_style, ">Trung bình ± SD</td>"))

            for (by_val1 in by_values1) {
              for (by_val2 in by_values2) {
                subset_data <- df[df[[by_var1]] == by_val1 & df[[by_var2]] == by_val2, ]

                # Kiểm tra có dữ liệu không rỗng
                if (nrow(subset_data) > 0) {
                  mean_val <- mean(subset_data[[var]], na.rm = TRUE)
                  sd_val <- sd(subset_data[[var]], na.rm = TRUE)

                  mean_str <- format_number(mean_val)
                  sd_str <- format_number(sd_val)

                  html <- c(html, paste0("<td class='center' colspan='2'>", mean_str, " ± ", sd_str, "</td>"))
                } else {
                  html <- c(html, "<td class='center' colspan='2'>-</td>")
                }
              }
            }

            html <- c(html, "</tr>")

            # Hàng trung vị (IQR)
            html <- c(html, paste0("<tr><td class='continuous-stats'", indent_style, ">Trung vị (IQR)</td>"))

            for (by_val1 in by_values1) {
              for (by_val2 in by_values2) {
                subset_data <- df[df[[by_var1]] == by_val1 & df[[by_var2]] == by_val2, ]

                # Kiểm tra có dữ liệu không rỗng
                if (nrow(subset_data) > 0 && sum(!is.na(subset_data[[var]])) > 0) {
                  median_val <- median(subset_data[[var]], na.rm = TRUE)
                  p25 <- quantile(subset_data[[var]], 0.25, na.rm = TRUE)
                  p75 <- quantile(subset_data[[var]], 0.75, na.rm = TRUE)

                  median_str <- format_number(median_val)
                  p25_str <- format_number(p25)
                  p75_str <- format_number(p75)

                  html <- c(html, paste0("<td class='center' colspan='2'>", median_str, " (", p25_str, " - ", p75_str, ")</td>"))
                } else {
                  html <- c(html, "<td class='center' colspan='2'>-</td>")
                }
              }
            }

            html <- c(html, "</tr>")

            # Thêm missing nếu có
            missing_counts <- matrix(0, nrow = length(by_values1), ncol = length(by_values2))
            for (i in seq_along(by_values1)) {
              for (j in seq_along(by_values2)) {
                subset_data <- df[df[[by_var1]] == by_values1[i] & df[[by_var2]] == by_values2[j], ]
                missing_counts[i, j] <- sum(is.na(subset_data[[var]]))
              }
            }

            if (sum(missing_counts) > 0) {
              html <- c(html, paste0("<tr><td class='continuous-stats'", indent_style, ">Missing</td>"))

              for (i in seq_along(by_values1)) {
                for (j in seq_along(by_values2)) {
                  subset_data <- df[df[[by_var1]] == by_values1[i] & df[[by_var2]] == by_values2[j], ]
                  miss_count <- missing_counts[i, j]
                  total_count <- nrow(subset_data)

                  if (total_count > 0) {
                    miss_percent <- 100 * miss_count / total_count
                    miss_percent_str <- format_number(miss_percent)

                    html <- c(html, paste0("<td class='number'>", miss_count, "</td><td class='number'>", miss_percent_str, " %</td>"))
                  } else {
                    html <- c(html, "<td class='number'>0</td><td class='number'>0.0 %</td>")
                  }
                }
              }

              html <- c(html, "</tr>")
            }
          } else {
            # Xử lý biến phân loại
            all_categories <- unique(na.omit(df[[var]]))

            # Kiểm tra xem biến có nhãn giá trị không
            value_labels <- attr(df[[var]], "labels")

            # Thêm missing vào danh sách danh mục nếu có
            if (any(is.na(df[[var]]))) {
              all_categories <- c(all_categories, NA)
            }

            # Xử lý từng danh mục
            for (cat in all_categories) {
              # Lấy tên hiển thị cho danh mục
              if (is.na(cat)) {
                display_cat <- "Missing"
              } else if (!is.null(value_labels)) {
                # Tìm nhãn cho giá trị này nếu có
                if (is.numeric(cat)) {
                  label_idx <- which(value_labels == cat)
                  display_cat <- if (length(label_idx) > 0) names(value_labels)[label_idx[1]] else cat
                } else {
                  display_cat <- cat
                }
              } else {
                display_cat <- cat
              }

              html <- c(html, paste0("<tr><td", indent_style, ">", display_cat, "</td>"))

              # Tính tần số và tỷ lệ cho mỗi nhóm chéo
              for (by_val1 in by_values1) {
                for (by_val2 in by_values2) {
                  subset_data <- df[df[[by_var1]] == by_val1 & df[[by_var2]] == by_val2, ]

                  if (is.na(cat)) {
                    freq <- sum(is.na(subset_data[[var]]))
                  } else {
                    freq <- sum(subset_data[[var]] == cat, na.rm = TRUE)
                  }

                  total <- nrow(subset_data)
                  if (total > 0) {
                    percent <- 100 * freq / total
                    percent_str <- format_number(percent)

                    html <- c(html, paste0("<td class='number'>", freq, "</td><td class='number'>", percent_str, " %</td>"))
                  } else {
                    html <- c(html, "<td class='number'>0</td><td class='number'>0.0 %</td>")
                  }
                }
              }

              html <- c(html, "</tr>")
            }
          }
        }, error = function(e) {
          warning(paste("Lỗi khi xử lý biến", var, ":", e$message))
        })
      }
    } else {
      # Báo lỗi nếu cung cấp quá nhiều biến by trong mode chéo
      stop("Hiện tại hàm chỉ hỗ trợ tối đa 2 biến by trong chế độ phân tầng chéo")
    }
  } else {
    # Tạo bảng thống kê mô tả không phân tầng
    html <- c(html, "<table>")
    html <- c(html, "<tr><th>Đặc điểm</th><th class='number'>Tần số</th><th class='number'>Tỷ lệ (%)</th></tr>")

    # Xử lý từng biến
    for (var in varlist) {
      tryCatch({
        # Lấy nhãn biến hoặc sử dụng tên biến nếu không có nhãn
        var_label <- attr(df[[var]], "label")
        if (is.null(var_label)) var_label <- var

        # Thêm hàng tiêu đề biến
        html <- c(html, paste0("<tr><td class='var-header'", indent_style, ">", var_label, "</td><td colspan='2'></td></tr>"))

        # Kiểm tra xem biến là liên tục hay phân loại
        unique_values <- unique(na.omit(df[[var]]))
        n_unique <- length(unique_values)
        N <- sum(!is.na(df[[var]]))

        # Kiểm tra xem biến có nhãn giá trị không
        has_value_labels <- !is.null(attr(df[[var]], "labels"))

        # Xác định xem biến có phải liên tục không
        is_continuous <- FALSE
        if (!has_value_labels) {
          if (n_unique >= 10) {
            # Kiểm tra nếu biến là numeric
            if (is.numeric(df[[var]])) {
              min_val <- min(df[[var]], na.rm = TRUE)
              max_val <- max(df[[var]], na.rm = TRUE)

              if (min_val >= 0 && max_val <= 100 && N > 0) {
                is_continuous <- (n_unique > 20)
              } else {
                is_continuous <- TRUE
              }
            }
          }
        }

        if (is_continuous) {
          # Thống kê tóm tắt cho biến liên tục
          mean_val <- mean(df[[var]], na.rm = TRUE)
          sd_val <- sd(df[[var]], na.rm = TRUE)
          median_val <- median(df[[var]], na.rm = TRUE)
          min_val <- min(df[[var]], na.rm = TRUE)
          max_val <- max(df[[var]], na.rm = TRUE)
          p25 <- quantile(df[[var]], 0.25, na.rm = TRUE)
          p75 <- quantile(df[[var]], 0.75, na.rm = TRUE)

          # Định dạng số thống kê với số chữ số thập phân đã chỉ định
          mean_str <- format_number(mean_val)
          sd_str <- format_number(sd_val)
          median_str <- format_number(median_val)
          min_str <- format_number(min_val)
          max_str <- format_number(max_val)
          p25_str <- format_number(p25)
          p75_str <- format_number(p75)

          # Thêm hàng cho thống kê liên tục
          html <- c(html,
                    paste0("<tr><td class='continuous-stats'", indent_style, ">Trung bình ± SD</td>",
                           "<td class='center' colspan='2'>", mean_str, " ± ", sd_str, "</td></tr>"),
                    paste0("<tr><td class='continuous-stats'", indent_style, ">Trung vị (IQR)</td>",
                           "<td class='center' colspan='2'>", median_str, " (", p25_str, " - ", p75_str, ")</td></tr>"))

          # Thêm số lượng missing cho biến liên tục
          miss_count <- sum(is.na(df[[var]]))
          if (miss_count > 0) {
            miss_percent <- 100 * miss_count / (N + miss_count)
            miss_percent_str <- format_number(miss_percent)
            html <- c(html,
                      paste0("<tr><td class='continuous-stats'", indent_style, ">Missing</td>",
                             "<td class='number'>", miss_count, "</td><td class='number'>", miss_percent_str, " %</td></tr>"))
          }
        } else if (is.character(df[[var]])) {
          # Cho biến chuỗi
          tbl <- table(df[[var]], useNA = "always")
          total_n <- sum(tbl)

          # Xử lý từng danh mục
          for (i in seq_along(tbl)) {
            cat_name <- names(tbl)[i]
            freq <- tbl[i]
            p <- 100 * freq / total_n
            p_str <- format_number(p)

            display_val <- if (is.na(cat_name)) "Missing" else cat_name

            html <- c(html,
                      paste0("<tr><td", indent_style, ">", display_val, "</td>",
                             "<td class='number'>", freq, "</td><td class='number'>", p_str, " %</td></tr>"))
          }
        } else {
          # Cho biến phân loại
          tbl <- table(df[[var]], useNA = "always")
          total_n <- sum(tbl)

          if (has_value_labels) {
            # Sử dụng nhãn giá trị nếu có
            labels <- attr(df[[var]], "labels")

            for (i in seq_along(tbl)) {
              cat_name <- names(tbl)[i]
              freq <- tbl[i]
              p <- 100 * freq / total_n
              p_str <- format_number(p)

              if (is.na(cat_name)) {
                display_val <- "Missing"
              } else {
                # Tìm nhãn cho giá trị này
                cat_val <- as.numeric(cat_name)
                label_idx <- which(labels == cat_val)
                if (length(label_idx) > 0) {
                  display_val <- names(labels)[label_idx[1]]
                } else {
                  display_val <- cat_name
                }
              }

              html <- c(html,
                        paste0("<tr><td", indent_style, ">", display_val, "</td>",
                               "<td class='number'>", freq, "</td><td class='number'>", p_str, " %</td></tr>"))
            }
          } else {
            # Không có nhãn giá trị, sử dụng giá trị như mặc định
            for (i in seq_along(tbl)) {
              cat_name <- names(tbl)[i]
              freq <- tbl[i]
              p <- 100 * freq / total_n
              p_str <- format_number(p)

              display_val <- if (is.na(cat_name)) "Missing" else cat_name

              html <- c(html,
                        paste0("<tr><td", indent_style, ">", display_val, "</td>",
                               "<td class='number'>", freq, "</td><td class='number'>", p_str, " %</td></tr>"))
            }
          }
        }
      }, error = function(e) {
        warning(paste("Lỗi khi xử lý biến", var, ":", e$message))
      })
    }
  }

  # Đóng bảng
  html <- c(html, "</table>")

  # Thêm tỷ lệ missing tổng thể nếu tùy chọn missing được chỉ định
  if (missing && total_cells > 0) {
    total_missing_percent <- 100 * total_missing / total_cells
    total_missing_percent_str <- format_number(total_missing_percent)
    html <- c(html, paste0("<div class='summary-text'>Tỷ lệ missing của toàn bộ dữ liệu: ",
                           total_missing_percent_str, "%</div>"))
  }

  # Thêm thông tin về dấu thập phân
  html <- c(html, paste0("<p><small id='decimal-info'>Dấu thập phân: dấu chấm (.)</small></p>"))

  # Thêm timestamp
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  html <- c(html, paste0("<p><small>Được tạo lúc: ", timestamp, "</small></p>"),
            "</div></body></html>")

  # Ghi HTML vào file
  writeLines(html, output)

  # Hiển thị đường dẫn đến file đầu ra
  full_path <- paste0("file://", normalizePath(output))
  message("\nBáo cáo đã được tạo: ", full_path)

  # Tự động mở file trong trình duyệt
  utils::browseURL(output)

  # Trả về kết quả
  return(list(output = output, timestamp = timestamp))
}


