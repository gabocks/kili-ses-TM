library(dplyr)
library(viridis)
library(lubridate)
library(tibble)
library(zoo)
library(ggplot2)
library(cli)
library(tidyr)
library(stringr)
library(scales)
library(ggtext)
library(numform)


kilanttrify<-
  function (project, spots = NULL, by_date = FALSE, exact_date = FALSE, 
          #plot_start_date = Sys.Date(), #colour_palette = ,
          set_date_limit=NULL,
          font_family = "sans", mark_quarters = FALSE, mark_years = FALSE, 
          size_wp = 6, show_wp = FALSE, hide_activities = FALSE, wp_label_bold = TRUE, 
          size_activity = 4, size_text_relative = 1, label_wrap = FALSE, 
          month_number_label = FALSE, month_label_string = "M", month_date_label = TRUE, 
          x_axis_position = "top", colour_stripe = "#dfdfdf",
          line_end = NULL, line_end_wp = "round", 
          line_end_activity = "butt", spot_padding = ggplot2::unit(0.2,"lines"),
          spot_fill = ggplot2::alpha(c("white"), 1), 
          spot_text_colour = "gray20", spot_size_text_relative = 1, 
          spot_fontface = "bold", spot_border = 0.25, month_breaks = 1, 
          show_vertical_lines = TRUE, axis_text_align = "right",
          within_SP=FALSE,within_SP_list=c("SP4"),show_dependencies=TRUE,#color_arrows="grey",
          alpha_wp=1,alpha_activity=0.7,fieldwork_alpha=0.7,only_fieldwork=FALSE,
          within_TaskForce=F,within_TF_list=c("PPGIS"))#GB 
{
  #project <- ::gantt_verify(project = project, by_date = by_date, 
   #                       exact_date = exact_date)
  if (show_wp & hide_activities) {
    cli::cli_abort("At least one of {.arg show_wp} or {.arg hide_activities} must be {.code TRUE}, otherwise there's nothing left to show.")
  }
######  
#color palette
  if (within_SP==F){
    set_colours<-c("SP1" = "#fbb4ae", "SP2" = "#b3cde3", "SP3" = "#ccebc5", "SP4" = "#decbe4","SP5"="#fed9a6","SP6"="#ffffcc","SP7"="#e5d8bd","past"="red","future"="grey","SP234"="#fddaec")
    } else if (length(within_SP_list)>1){
      set_colours<-c("SP1" = "#fbb4ae", "SP2" = "#b3cde3", "SP3" = "#ccebc5", "SP4" = "#decbe4","SP5"="#fed9a6","SP6"="#ffffcc","SP7"="#e5d8bd","past"="red","future"="grey","SP234"="#fddaec")
    } else  {
      project<-filter(project,SP%in%within_SP_list)
      set_colours<-viridis::viridis(length(unique(project$wp))) #wesanderson::wes_palette("Darjeeling1")
      names(set_colours) <- unique(project$wp)
      set_colours<-c(set_colours,"past"="red","future"="grey")
  }
  ##
  if (only_fieldwork==TRUE){
    project$activity_pred <- gsub(" ","",project$activity_pred)
    fieldwork_activity_pred<-na.omit(unlist(strsplit(project$activity_pred[which(project$type=="fieldwork")],split=";")))
    project<-filter(project,type=="fieldwork")%>%rbind(filter(project,activity_id%in%fieldwork_activity_pred))
  }
  ##
  if (within_TaskForce==T){
    project$activity_pred <- gsub(" ","",project$activity_pred)
    TF_activity_pred<-na.omit(unlist(strsplit(project$activity_pred[which(project$taskforce==within_TF_list)],split=";")))
    project<-dplyr::filter(project,taskforce%in%within_TF_list)%>%rbind(filter(project,activity_id%in%TF_activity_pred))
  }
  ##
    #####
    ## set start and end date of the plot
    if (is.null(set_date_limit)) {
      plot_start_date<-floor_date(ymd(min(project$start_date)),unit = "month")-1
      plot_end_date<-ceiling_date(ymd(max(project$end_date)),unit = "month")
    } else {
    
    project<-bind_rows(filter(project,start_date>=set_date_limit[1]& end_date<=set_date_limit[2]),
                       filter(project,start_date>=set_date_limit[1] & start_date<=set_date_limit[2]),
                       filter(project,start_date<=set_date_limit[1] & end_date>=set_date_limit[1]),
                       filter(project,start_date<=set_date_limit[1] & end_date>=set_date_limit[2]))
    
    plot_start_date<-floor_date(ymd(min(project$start_date)),unit = "month")-1
    plot_end_date<-ceiling_date(ymd(max(project$end_date)),unit = "month")
    
    
    }
    ##
   if (is.null(line_end) == FALSE) {
    line_end_wp <- line_end
    line_end_activity <- line_end
  }
# transform dates based on options
 #removed line 40-82
  #if (exact_date == TRUE) {
    df <- project %>% dplyr::arrange(SP) %>% dplyr::mutate(start_date = as.Date(start_date), 
                                    end_date = as.Date(end_date), wp = as.character(wp), 
                                    activity = as.character(activity))#,
                                   # pred_end_date=as.Date(pred_end_date))#GB
    df_yearmon <- df %>% dplyr::mutate(start_date = zoo::as.Date(zoo::as.yearmon(start_date),frac = 0),
                                       end_date = zoo::as.Date(zoo::as.yearmon(end_date),frac = 1))#,
                                      # pred_end_date = zoo::as.Date(pred_end_date,frac = 1)) #GB
 # }
  
# sequence_months is the sequence of the all the year-months in the project
  sequence_months <- seq.Date(from = min(df_yearmon[["start_date"]]), 
                              to = max(df_yearmon[["end_date"]]), by = "1 month")
  if (length(sequence_months)%%2 != 0) {
    sequence_months <- seq.Date(from = min(df_yearmon[["start_date"]]), 
                                to = max(df_yearmon[["end_date"]]) + 1, by = "1 month")
  }
  # numeric matrix of sequence_months
  date_range_matrix <- matrix(as.numeric(sequence_months),ncol = 2, byrow = TRUE)
  # as a matrix
  date_range_df <- tibble::tibble(start = zoo::as.Date.numeric(date_range_matrix[,1]), end = zoo::as.Date.numeric(date_range_matrix[, 2]))
  # creating the appropriate breaks
  date_breaks <- zoo::as.Date(zoo::as.yearmon(seq.Date(from = min(df_yearmon[["start_date"]] + 15), to = max(df_yearmon[["end_date"]] + 15), by = paste(month_breaks,  "month"))), frac = 0.5)
  # creating the quarter breaks                                                                                                                         
  date_breaks_q <- seq.Date(from = lubridate::floor_date(x = min(df_yearmon[["start_date"]]),unit = "year"), to = lubridate::ceiling_date(x = max(df_yearmon[["end_date"]]), unit = "year"), by = "1 quarter")
  # creating the year breaks                                                                                                    
  date_breaks_y <- seq.Date(from = lubridate::floor_date(x = min(df_yearmon[["start_date"]]),unit = "year"), to = lubridate::ceiling_date(x = max(df_yearmon[["end_date"]]), unit = "year"), by = "1 year")
  # levels of the wp
if (within_SP==F){  
  distinct_yearmon_levels_df <- df_yearmon %>% 
                                            # make a list of the activity
                                            dplyr::distinct(SP,wp,activity) %>% 
                                            tidyr::unite(col = "wp_activity", wp, activity,remove = FALSE, sep = "_") %>% 
                                            dplyr::group_by(SP,wp) %>% 
                                            dplyr::summarise(wp_activity = list(wp_activity)) %>% 
                                             # add the list of WP
                                           # dplyr::left_join(x = tibble::tibble(wp = unique(df_yearmon[["wp"]])), by = "wp") %>% 
                                            dplyr::group_by(SP,wp) %>% 
                                            dplyr::mutate(wp = stringr::str_c("wp",SP, wp, sep = "_")) %>% 
                                            dplyr::ungroup() #%>%
                                           # dplyr::mutate(gantt_colour = colour_palette) #GB remove color
} else{
  distinct_yearmon_levels_df <- df_yearmon %>% 
    # make a list of the activity
    dplyr::distinct(SP,wp,activity) %>% 
    tidyr::unite(col = "wp_activity", wp, activity,remove = FALSE, sep = "_") %>% 
    dplyr::group_by(SP,wp) %>% 
    dplyr::summarise(wp_activity = list(wp_activity)) %>% 
    # add the list of WP
    # dplyr::left_join(x = tibble::tibble(wp = unique(df_yearmon[["wp"]])), by = "wp") %>% 
    dplyr::group_by(SP,wp) %>% 
    dplyr::mutate(wp = stringr::str_c(wp, wp, sep = "_")) %>% 
    dplyr::ungroup() #%>%
  # dplyr::mutate(gantt_colour = colour_palette) #GB remove color
}
   
# set labels
  distinct_yearmon_labels_df <- df_yearmon %>% dplyr::distinct(SP,wp, activity) %>% dplyr::group_by(SP,wp) %>% dplyr::summarise(activity = list(activity)) %>% 
    dplyr::ungroup()# %>% dplyr::left_join(x = tibble::tibble(wp = unique(df_yearmon[["wp"]])),by = "wp")
 #
   if (wp_label_bold) {
    distinct_yearmon_labels_df <- distinct_yearmon_labels_df %>% 
      dplyr::mutate(wp = stringr::str_c("<b>", wp, "</b>"))
    if (is.null(spots) == FALSE) {
      wp_v <- project %>% dplyr::distinct(wp) %>% dplyr::pull(wp)
      spots[["activity"]][spots[["activity"]] %in% wp_v] <- stringr::str_c("<b>", spots[["activity"]][spots[["activity"]] %in% wp_v], "</b>")
    }
  }
  # create levels for labels
  level_labels_df <- tibble::tibble(levels = rev(unlist(t(matrix(c(distinct_yearmon_levels_df$wp, 
                                                                   distinct_yearmon_levels_df$wp_activity), ncol = 2)))), 
                                    labels = rev(unlist(t(matrix(c(distinct_yearmon_labels_df$wp, 
                                                                   distinct_yearmon_labels_df$activity), ncol = 2)))))
    
  if (label_wrap != FALSE) {
    if (isTRUE(label_wrap)) {
      label_wrap <- 32
    }
    level_labels_df$labels <- stringr::str_wrap(string = level_labels_df$labels, 
                                                width = label_wrap)
    level_labels_df$labels <- stringr::str_replace_all(string = level_labels_df$labels,pattern = "\n", replacement = "<br />")
    
# spots?    
    if (is.null(spots) == FALSE) {
      spots$activity <- stringr::str_wrap(string = spots$activity, 
                                          width = label_wrap)
      spots$activity <- stringr::str_replace_all(string = spots$activity, 
                                                 pattern = "\n", replacement = "<br />")
    }
  }
# creating final df with all necessary for plotting
  if (within_SP == FALSE) {
    
    df_yearmon_fct <- dplyr::bind_rows(activity = df,wp = df %>% 
                                         dplyr::group_by(SP,wp) %>% 
                                         dplyr::summarise(activity = unique(wp), start_date = min(start_date), end_date = max(end_date)) %>% 
                                         dplyr::mutate(wp = stringr::str_c("wp",SP, sep = "_")), 
                                       .id = "segment_type") %>%
                                          dplyr::arrange(SP,wp,activity)%>%
                                          tidyr::unite(col = "activity", wp, activity, remove = FALSE) %>% 
                                          #dplyr::left_join(y = distinct_colours_df, by = "activity") %>% 
                                          dplyr::mutate(activity = factor(x = activity,levels = level_labels_df$levels))%>%
                                          dplyr::mutate(tip_names=paste0(SP, "\n Task: ", activity, "\n Responsible: ", Responsible))
 } else {
    df_yearmon_fct <- dplyr::bind_rows(activity = df,wp = df %>% 
                        dplyr::group_by(SP,wp) %>% 
                       dplyr::summarise(activity = unique(wp), start_date = min(start_date), end_date = max(end_date)),# %>% 
                       # dplyr::mutate(wp = stringr::str_c("wp",SP, sep = "_")),
                       .id = "segment_type") %>%
                        dplyr::arrange(SP,wp,activity)%>%
                        tidyr::unite(col = "activity", wp, activity, remove = FALSE) %>% 
                        #dplyr::left_join(y = distinct_colours_df, by = "activity") %>% 
                        dplyr::mutate(activity = factor(x = activity,levels = level_labels_df$levels))%>%
                        dplyr::mutate(tip_names=paste0(SP, "\n Task: ", activity, "\n Responsible: ", Responsible)) 
  }
 
################
# plot only wp or only activities    
  if (show_wp == FALSE) {
    df_yearmon_fct <- df_yearmon_fct %>% dplyr::filter(segment_type != "wp")
  }
  if (hide_activities == TRUE) {
    df_yearmon_fct <- df_yearmon_fct %>% dplyr::filter(segment_type != "activity")
  }
#######################
  if (within_SP==F){
  gg_gantt <- ggplot2::ggplot(data = df_yearmon_fct, mapping = ggplot2::aes(x = start_date, y = activity, xend = end_date, yend = activity, colour = SP)) + 
    ggplot2::geom_rect(data = date_range_df, ggplot2::aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), inherit.aes = FALSE, fill = colour_stripe)+#,alpha = factor(0.4)
    ggplot2::scale_colour_manual(values = set_colours,na.translate = F)
  }else{
    gg_gantt <- ggplot2::ggplot(data = df_yearmon_fct, mapping = ggplot2::aes(x = start_date, y = activity, xend = end_date, yend = activity, colour = wp)) + 
      ggplot2::geom_rect(data = date_range_df, ggplot2::aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), inherit.aes = FALSE, fill = colour_stripe)+#+ ,alpha = factor(0.4)
      ggplot2::scale_colour_manual(values = set_colours,na.translate = F)
  }
 
# plot with separating by quarters or years     
  if (mark_quarters == TRUE) {
    gg_gantt <- gg_gantt + ggplot2::geom_vline(xintercept = date_breaks_q,colour = "gray50")
  }
  if (mark_years == TRUE) {
    gg_gantt <- gg_gantt + ggplot2::geom_vline(xintercept = date_breaks_y,colour = "gray50")
  }
########################################
# 
  if (utils::packageVersion("ggplot2") > "3.3.6") {
    gg_gantt <- gg_gantt +ggplot2::geom_segment(data = df_yearmon_fct, lineend = line_end_activity, linewidth = size_activity,color="black",aes(alpha = ifelse(type == "fieldwork" & !is.na(type), fieldwork_alpha, 0))) + 
      ggiraph::geom_segment_interactive(data = df_yearmon_fct, lineend = line_end_activity, linewidth = size_activity,aes(alpha=ifelse(segment_type == "activity" & !is.na(segment_type), alpha_activity, 0),tooltip=tip_names,data_id=activity))#aes(alpha=ifelse(segment_type == "activity", alpha_activity, 0))
      
      if (sum(is.na(df_yearmon_fct$completed))>0) {
        gg_gantt <- gg_gantt +ggplot2::geom_segment(data = filter(df_yearmon_fct,completed=="y"), lineend = line_end_activity, linewidth = size_activity,color="black",alpha=1)
      }
        if(show_wp==TRUE){
      gg_gantt <- gg_gantt +ggplot2::geom_segment(data = df_yearmon_fct, lineend = line_end_wp, linewidth = size_wp,aes(alpha = ifelse(segment_type ==  "wp", alpha_wp, 0)))
    }
    }else {
    gg_gantt <- gg_gantt + ggplot2::geom_segment(data = df_yearmon_fct, lineend = line_end_activity, linewidth = size_activity,color="black",aes(alpha = ifelse(type == "fieldwork", fieldwork_alpha, 0))) + 
      ggiraph::geom_segment_interactive(data = df_yearmon_fct,lineend = line_end_activity, linewidth = size_activity,aes(alpha=ifelse(segment_type == "activity", alpha_activity, 0),tooltip=tip_names,data_id=activity))
      if(show_wp==TRUE){                    
        gg_gantt <- gg_gantt + ggplot2::geom_segment(data = df_yearmon_fct,lineend = line_end_wp, linewidth = size_wp,aes(alpha = ifelse(segment_type ==  "wp", alpha_wp, 0)))
      }
    }
# Scale_x_date depending on label
  if (month_number_label == TRUE & month_date_label == TRUE) {
    gg_gantt <- gg_gantt + ggplot2::scale_x_date(name = NULL, breaks = date_breaks,date_labels = "%b\n%Y" , minor_breaks = NULL, #
                                                 sec.axis = ggplot2::dup_axis(labels = paste0(month_label_string, 
                                                 seq_along(date_breaks) * month_breaks - (month_breaks -1))),limits = c(ymd(plot_start_date),ymd(plot_end_date)))
  
    }else if (month_number_label == FALSE & month_date_label ==TRUE) {
    gg_gantt <- gg_gantt + ggplot2::scale_x_date(name = NULL, 
                                                 breaks = date_breaks,labels =scales::label_date_short(format = c("%Y", "%b"),sep = "\n"), minor_breaks = NULL,#f_month,
                                                 position = x_axis_position,limits = c(ymd(plot_start_date),ymd(plot_end_date)))
 
     }else if (month_number_label == TRUE & month_date_label == FALSE) {
    gg_gantt <- gg_gantt + ggplot2::scale_x_date(name = NULL, 
                                                 breaks = date_breaks, date_labels = paste0(month_label_string, 
                                                 seq_along(date_breaks) * month_breaks - (month_breaks - 
                                                 1)), minor_breaks = NULL, position = x_axis_position,limits = c(ymd(plot_start_date),ymd(plot_end_date)))
  
    }else if (month_number_label == FALSE & month_date_label == FALSE) {
    gg_gantt <- gg_gantt + ggplot2::scale_x_date(name = NULL,limits = c(ymd(plot_start_date),ymd(plot_end_date)))
    } 

    # axis_text_align
  if (axis_text_align == "right") {
    axis_text_align_n <- 1
  } else if (axis_text_align == "centre" | axis_text_align == 
           "center") {
    axis_text_align_n <- 0.5
  }else if (axis_text_align == "left") {
    axis_text_align_n <- 0
  }else {
    axis_text_align_n <- 1
  }

# scale_y_discrete and theme
  gg_gantt <- gg_gantt + ggplot2::scale_y_discrete(name = NULL, breaks = level_labels_df$levels, labels = level_labels_df$labels) + 
                          ggplot2::theme_minimal() + 
                         # ggplot2::scale_colour_manual(values = set_colours) + 
                          ggplot2::theme(text = ggplot2::element_text(family = font_family),axis.text.y.left = ggtext::element_markdown(size = ggplot2::rel(size_text_relative), 
                                         hjust = axis_text_align_n), axis.text.x = ggplot2::element_text(size = ggplot2::rel(size_text_relative)),legend.position = "bottom")

# remove alpha legend
  gg_gantt <- gg_gantt+scale_alpha_identity(guide = 'none') 

  
  # GB # add dependencies
  if (show_dependencies==TRUE) {
    # filter only dependencies
    XX<-drop_na(df_yearmon_fct,activity_pred)
    XX$activity_pred <- gsub(" ","",XX$activity_pred)
    if (sum(grepl(";",XX$activity_pred))>0){
    XX<-uncount(XX,lengths(strsplit(unlist(activity_pred),split=";")))%>%mutate(activity_pred=as.numeric(unlist(strsplit(unlist(XX$activity_pred),split=";"))))
    }
    
    ## add the proper activity name to activity_pred and date
    XX$pred_end_date<-df_yearmon_fct$end_date[match(XX$activity_pred,df_yearmon_fct$activity_id,incomparables =NA)]
    XX$activity_pred<-df_yearmon_fct$activity[match(XX$activity_pred,df_yearmon_fct$activity_id,incomparables =NA)]
    XX$arrow_color<-ifelse(XX$pred_end_date>XX$end_date,"past","future")
  gg_gantt<-gg_gantt+ggplot2::geom_curve(data=XX,mapping = ggplot2::aes(x = pred_end_date,y = activity_pred, xend = start_date, yend = activity,color=arrow_color),linewidth=0.3,curvature=0.2,arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm")),na.rm = TRUE)#colour=color_arrows
  }
  ###########
  
 # return(gg_gantt)
  return(girafe(ggobj = gg_gantt))
  }
