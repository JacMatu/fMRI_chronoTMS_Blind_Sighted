##---------- PLOT FUNCTIONS for fMRI_chronoTMS_Blind_Sighted repo ----------

## ---------- BEHAVIOUR ----------
# Reading speed, scanner performance, MNI coordinates (maybe?)

#Reading speed jitter with means & SE
# since it's a single plot everything is HARDCODED, it's here for 
# future references and clean plots code only
beh_reading_speed_points <- function (data) {
    
    #Load color palets
    dark_colors <- brewer.pal(n=3, name = "Dark2") 
    light_colors <- brewer.pal(n=3, name = "Pastel2") 
    
    #LEGEND generated automatically, (re)move it with function call
    # e.g. plot <- beh_reading_speed_points(data = DF) + 
            # theme(legend.position = "none")
    
    data %>% 
        ggplot(aes(x = lexicality, y = speed, 
                   group = interaction(group, lexicality), 
                   color = group)) +
        geom_point(position = position_jitterdodge(jitter.width=0.15,
                                                   dodge.width = 0.5),
                   size = 2.5) +
        stat_summary(fun = "mean", 
                     geom = "crossbar", 
                     position = position_dodge(width = 0.5),
                     width = .45, 
                     color = rep(c(dark_colors[1], dark_colors[2]),2)) +
        stat_summary(fun.max = function(x) mean(x) + (sd(x)/sqrt(20)),
                     fun.min = function(x) mean(x) - (sd(x)/sqrt(20)),
                     geom = "errorbar",
                     position = position_dodge(width = 0.5),
                     width = .15,
                     size = 0.8,
                     color = rep(c(dark_colors[1], dark_colors[2]),2),
                     show.legend = FALSE) +
        scale_color_manual(values = c(light_colors[1], light_colors[2])) +
        scale_y_continuous(name = "Reading speed [items per minute]") +
        scale_x_discrete(name = "Lexicality") +
        labs(color="Group") +
        theme_cowplot(font_size = 16, font_family = "Arial") +
        theme(axis.line = element_line(colour = 'black', size = 1),
              axis.ticks = element_line(colour = 'black', size = 1),
              axis.text = element_text(face="bold"),
              legend.title = element_text(face = "bold"))
}

#Points with mean bars, CPP-style
beh_task_points <- function(data, mod) {
    
    dark_colors <- brewer.pal(n=3, name = "Dark2") 
    light_colors <- brewer.pal(n=3, name = "Pastel2") 
    
    data %>% 
        filter(modality == mod) %>% 
        mutate(condition = factor(condition, levels=c("Words", "Pseudowords", "Control"))) %>%
        ggplot(aes(x = condition, y = d_prime, 
                   group = interaction(group, condition), 
                   color = group)) +
        #color points
        geom_point(position = position_jitterdodge(jitter.width=0.15,
                                                   dodge.width = 0.75),
                   size = 2.5) + 
        geom_hline(yintercept = 4.652696, linetype = "dotted") +
        annotate("text", x = "Control", y = 4.652696, label = "Max accuracy", vjust = -0.5) +
        #black borders around points?
       #geom_point(shape = 1, size = 2, position = position_dodge2(width = 0.4), colour = "black") + 
        # MEAN CROSSBAR
        stat_summary(fun = "mean", 
                     geom = "crossbar", 
                     position = position_dodge(width = 0.75),
                     width = .45, 
                    # size = 1,
                     color = rep(c(dark_colors[1], dark_colors[2]),3)) +
        # SEM ERROR BARS
        stat_summary(fun.max = function(x) mean(x) + (sd(x)/sqrt(20)),
                     fun.min = function(x) mean(x) - (sd(x)/sqrt(20)),
                     geom = "errorbar",
                     position = position_dodge(width = 0.75),
                     width = .15,
                     size = 0.8,
                     #color = "black",
                     color = rep(c(dark_colors[1], dark_colors[2]),3),
                     show.legend = FALSE) +
        scale_color_manual(values = c(light_colors[1], light_colors[2])) +
        scale_y_continuous(name = "Task performance [d']") +
        scale_x_discrete(name = "Condition") +
        theme_cowplot(font_size = 16, font_family = "Arial") +
        theme(axis.line = element_line(colour = 'black', size = 1),
              axis.ticks = element_line(colour = 'black', size = 1),
              axis.text = element_text(face="bold"),
              legend.position = "none")
        
}

## ---------- FMRI ---------- 

#Grouped by Modality and condition, group specific! 
#CPP-lab style: points + mean + SEM
fMRI_ROI_points <- function(data, roi, sub_group) {
    
    #Prepare triple colour schemes for groups
    blind_triple_colors <- c(brewer.pal(3, "Pastel2")[1], 
                             brewer.pal(3, "Set2")[1],   
                             brewer.pal(3, "Dark2")[1],  
                             brewer.pal(3, "Pastel2")[1], 
                             brewer.pal(3, "Set2")[1],   
                             brewer.pal(3, "Dark2")[1])
    
    sighted_triple_colors <- c(brewer.pal(3, "Pastel2")[2], 
                               brewer.pal(3, "Set2")[2],   
                               brewer.pal(3, "Dark2")[2],  
                               brewer.pal(3, "Pastel2")[2], 
                               brewer.pal(3, "Set2")[2],   
                               brewer.pal(3, "Dark2")[2])
    
    # Y Axis label for BOLD 
    bold_label <- "BOLD contrast estimate (a.u.)"
    
    # Prepare legend labels? 
    legend_labels <- if(sub_group == "Blind"){
        c("Control (Blind)", "Pseudowords (Blind)", "Words (Blind)")
    } else if (sub_group == "Sighted"){
        c("Control (Sighted)", "Pseudowords (Sighted)", "Words (Sighted)")
    }
    
    #Main plot
    data %>% 
        filter(Group == sub_group) %>% 
        filter(ROI == roi) %>% 
        mutate(Condition = factor(Condition, levels=c("Control", "Pseudowords", "Words"))) %>%
        ggplot(aes(x = Modality, y = Contrast_Estimate, group = interaction(Modality, Condition), 
                   color = Condition)) +
        geom_hline(yintercept = 0, linetype = "dotted") +
        geom_point(size = 3,
                   alpha = 0.8,
                   position = position_jitterdodge(jitter.width=0.2,
                                                   dodge.width = 1)) +
        stat_summary(fun = "mean",
                     geom = "crossbar",
                     position = position_dodge(width = 1),
                     width = .75,
                     size = 1,
                     show.legend = FALSE) +
        stat_summary(fun.max = function(x) mean(x) + (sd(x)/sqrt(20)),
                     fun.min = function(x) mean(x) - (sd(x)/sqrt(20)),
                     geom = "errorbar",
                     position = position_dodge(width = 1),
                     width = .15,
                     size = 0.8,
                     color = "black",
                     show.legend = FALSE) +
        scale_color_manual(values = if(sub_group == "Blind"){
                                 blind_triple_colors
                           } else if (sub_group == "Sighted"){
                                sighted_triple_colors
                           },
                           labels = legend_labels) +

        scale_y_continuous(limits = c(-15, 20), 
                           name = bold_label) +
        scale_x_discrete(name = "Modality") +
        theme_cowplot(font_size = 20, font_family = "Arial") +
        if(sub_group == "Blind") {
            theme(axis.line = element_line(colour = 'black', size = 1),
                  axis.ticks = element_line(colour = 'black', size = 1),
                  axis.text = element_text(face="bold"),
              #    legend.position = "none",
                  legend.title = element_blank())}
        else if(sub_group == "Sighted") {
            theme(axis.line = element_line(colour = 'black', size = 1),
              axis.ticks = element_line(colour = 'black', size = 1),
              axis.text = element_text(face="bold"),
              axis.line.y = element_line(colour = 'black', size = 0),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              axis.title.y = element_blank(),
             # legend.position = "none",
              legend.title = element_blank())
    }
    
}

## ---------- TMS ----------

## MAIN PLOT FOR DATA ACROSS SITES AND GORUPS: MODALITIES SEPARATE
# Arguments: DATA_FRAME, SUB_GROUP ["Blind" or "Sighted"], 
#            METRIC ["RT", "Accuracy"], MOD ["read" or "speech"]
# Y AXES ARE NOT FIXED ACROSS PLOTS! 

#RAW DATA
tms_line_mod_sep_raw <- function (data, sub_group, metric, mod) {
    
    # Prepare variables with labels and colors
    dark_colors <- brewer.pal(n=3, name = "Dark2") 
    light_colors <- brewer.pal(n=3, name = "Pastel2") 
    
    
    #Legend labels: here group and sensory modalities
    #legend_labels <- if(sub_group == "Blind"){
    #    c("Words (Blind)", "Pseudowords (Blind)")
    #} else if (sub_group == "Sighted"){
    #    c("Words (Sighted)", "Pseudowords (Sighted)")}
    
    # Axis labels, y axis depends on the chosen metric, x describes time windows
    tms_y_label <- if(metric == "RT"){
        "Reaction Time TMS Effect [ms]"}
    else if (metric == "Accuracy"){
        "Accuracy rates TMS Effect [%]"}
    
    tms_x_labels <- c("60-100 ms", "160-200 ms", "260-300 ms")
    
    ## BASIC LINE PLOTS - DOUBLE MODALITIES
    v1_line <- data %>%
        filter(Group == sub_group & Site == "EVC" & Modality == mod) %>%
        ggplot(aes(x = TW, y = mean, group = 1, color = sub_group)) +
        geom_line(size = 1) +
        geom_point(size = 4) +
        geom_errorbar(aes(ymin = mean-sem, ymax = mean+sem), 
                      position=position_dodge(0), width = 0.2, size = 1, 
                      color = if(sub_group == "Blind") {
                          rep(dark_colors[1],3)
                      } else if(sub_group == "Sighted") {
                          rep(dark_colors[2],3)
                      }) +
        geom_hline(yintercept=0, linetype='dashed', col = 'black',size = 1) +
        ylab(tms_y_label) +
        xlab("Time Window") +
        scale_y_continuous(limits = if(sub_group == "Blind" & metric == "RT" & mod == "read"){
                c(-50, 250)}
            else if(sub_group == "Sighted" & metric == "RT" & mod == "read"){
                c(0, 70)
            }
            else if(metric == "RT" & mod == "speech"){
                c(-40, 60)
            }
            else if(metric == "Accuracy"){
                c(-10, 12)
            }) +
        scale_x_discrete(labels = tms_x_labels) +
        scale_color_manual(values = if(sub_group == "Blind") {
            dark_colors[1]
        } else if(sub_group == "Sighted") {
            dark_colors[2]},
        labels = sub_group) + #only label lines with GROUP, modalities will be on separate panels
        #theme_cowplot(font_size = 16, font_family = "Arial") +
        theme_cowplot(font_size = 20, font_family = "Arial") +
        theme(axis.line = element_line(colour = 'black', size = 1),
                 axis.ticks = element_line(colour = 'black', size = 1),
                  axis.text = element_text(face="bold"),
                  legend.title = element_blank())
    
    
    #Prepare basic "trimmed" vwfa (right side of the plot)
    vwfa_line <- data %>%
        filter(Group == sub_group & Site == "VWFA" & Modality == mod) %>%
        ggplot(aes(x = TW, y = mean, group = 1, color = sub_group)) +
        geom_line(size = 1) +
        geom_point(size = 4) +
        geom_errorbar(aes(ymin = mean-sem, ymax = mean+sem), 
                      position=position_dodge(0), width = 0.2, size = 1, 
                      color = if(sub_group == "Blind") {
                          rep(dark_colors[1],3)
                      } else if(sub_group == "Sighted") {
                          rep(dark_colors[2],3)
                      }) +
        geom_hline(yintercept=0, linetype='dashed', col = 'black',size = 1) +
        ylab(tms_y_label) +
        xlab("Time Window") +
        scale_y_continuous(limits = if(sub_group == "Blind" & metric == "RT" & mod == "read"){
            c(-50, 250)}
            else if(sub_group == "Sighted" & metric == "RT" & mod == "read"){
                c(0, 70)
            }
            else if(metric == "RT" & mod == "speech"){
                c(-40, 60)
            }
            else if(metric == "Accuracy"){
                c(-10, 12)
            }) +
        scale_x_discrete(labels = tms_x_labels) +
        scale_color_manual(values = if(sub_group == "Blind") {
            dark_colors[1]
        } else if(sub_group == "Sighted") {
            dark_colors[2]},
        labels = sub_group) + #only label lines with GROUP, modalities will be on separate panels
        #theme_cowplot(font_size = 16, font_family = "Arial") +
        theme_cowplot(font_size = 20, font_family = "Arial") +
        theme(axis.line = element_line(colour = 'black', size = 1),
              axis.ticks = element_line(colour = 'black', size = 1),
              axis.text = element_text(face="bold"),
              axis.line.y = element_line(colour = 'black', size = 0),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              axis.title.y = element_blank(),
              legend.title = element_blank())
    
    
    
    
    # Paste the plots together! 
    # Legends are centered when plots are pasted across groups 
    # e.g. plots have 4 x 1 layout, legends are in column 2 (blind) and 3 (sighted)
    
    
    # SEPARATE PLOTS = LEGENDS NOT NEEDED? 
    
    (v1_line + theme(legend.position= "none")) +
        (vwfa_line + theme(legend.position= "none"))
    
    #IN CASE LEGENDS ARE NEEDED:     
    
    # if(sub_group == "Blind") {
    #     
    #     (v1_line + theme(legend.position= "none")) + 
    #      #   (vwfa_line + theme(legend.position= "bottom")) +
    #         (vwfa_line + theme(legend.position= "none")) +
    #         plot_layout(ncol = 2) 
    #     
    # }else if(sub_group == "Sighted") {
    #     (v1_line + theme(legend.position= "bottom")) + 
    #       #  (vwfa_line + theme(legend.position= "none")) +
    #         (vwfa_line + theme(legend.position= "none")) +
    #         plot_layout(ncol = 2) 
    # }
    
}

#GLM DATA (EMMEANS)
tms_line_mod_sep_glm <- function (data, sub_group, metric, mod) {
    
    # Prepare variables with labels and colors
    dark_colors <- brewer.pal(n=3, name = "Dark2") 
    light_colors <- brewer.pal(n=3, name = "Pastel2") 
    
    
    # Axis labels, y axis depends on the chosen metric, x describes time windows
    tms_y_label <- if(metric == "RT"){
        "Reaction Time TMS Effect [ms]"}
    else if (metric == "Accuracy"){
        "Accuracy rate TMS Effect [logit]"}
    
    tms_x_labels <- c("60-100 ms", "160-200 ms", "260-300 ms")
    
    ## BASIC LINE PLOTS - DOUBLE MODALITIES
    v1_line <- data %>%
        filter(Group == sub_group & Site == "EVC" & Modality == mod) %>%
       # mutate(TW = factor(TW, levels = c("earlyTMS", "midTMS", "lateTMS"))) %>% 
        ggplot(aes(x = TW, y = estimate, group = 1, color = sub_group)) +
        geom_line(size = 1) +
        geom_point(size = 4) +
        geom_errorbar(aes(ymin = estimate-SE, ymax = estimate+SE), 
                      position=position_dodge(0), width = 0.2, size = 1, 
                      color = if(sub_group == "Blind") {
                          rep(dark_colors[1],3)
                      } else if(sub_group == "Sighted") {
                          rep(dark_colors[2],3)
                      }) +
        geom_hline(yintercept=0, linetype='dashed', col = 'black',size = 1) +
        ylab(tms_y_label) +
        xlab("Time Window") +
        scale_y_continuous(limits = if(sub_group == "Blind" & metric == "RT" & mod == "read"){
            c(-50, 250)}
            else if(sub_group == "Sighted" & metric == "RT" & mod == "read"){
                c(0, 80)
            }
            else if(metric == "RT" & mod == "speech"){
                c(-40, 60)
            }
            else if(metric == "Accuracy"){
                #c(-12, 12) #RAW % ERRORS
                c(-1.5, 2) #LOGIT GLM VALUES
            }) +
        scale_x_discrete(labels = tms_x_labels) +
        scale_color_manual(values = if(sub_group == "Blind") {
            dark_colors[1]
        } else if(sub_group == "Sighted") {
            dark_colors[2]},
        labels = sub_group) + #only label lines with GROUP, modalities will be on separate panels
        #theme_cowplot(font_size = 16, font_family = "Arial") +
        theme_cowplot(font_size = 20, font_family = "Arial") +
        theme(axis.line = element_line(colour = 'black', size = 1),
              axis.ticks = element_line(colour = 'black', size = 1),
              axis.text = element_text(face="bold"),
              legend.title = element_blank())
    
    
    #Prepare basic "trimmed" vwfa (right side of the plot)
    vwfa_line <- data %>%
        filter(Group == sub_group & Site == "VWFA" & Modality == mod) %>%
       # mutate(TW = factor(TW, levels = c("earlyTMS", "midTMS", "lateTMS"))) %>% 
        ggplot(aes(x = TW, y = estimate, group = 1, color = sub_group)) +
        geom_line(size = 1) +
        geom_point(size = 4) +
        geom_errorbar(aes(ymin = estimate-SE, ymax = estimate+SE), 
                      position=position_dodge(0), width = 0.2, size = 1, 
                      color = if(sub_group == "Blind") {
                          rep(dark_colors[1],3)
                      } else if(sub_group == "Sighted") {
                          rep(dark_colors[2],3)
                      }) +
        geom_hline(yintercept=0, linetype='dashed', col = 'black',size = 1) +
        ylab(tms_y_label) +
        xlab("Time Window") +
        scale_y_continuous(limits = if(sub_group == "Blind" & metric == "RT" & mod == "read"){
            c(-50, 250)}
            else if(sub_group == "Sighted" & metric == "RT" & mod == "read"){
                c(0, 80)
            }
            else if(metric == "RT" & mod == "speech"){
                c(-40, 60)
            }
            else if(metric == "Accuracy"){
                #c(-12, 12) #RAW % ERRORS
                c(-1.5, 2) #LOGIT GLM VALUES
            }) +
        scale_x_discrete(labels = tms_x_labels) +
        scale_color_manual(values = if(sub_group == "Blind") {
            dark_colors[1]
        } else if(sub_group == "Sighted") {
            dark_colors[2]},
        labels = sub_group) + #only label lines with GROUP, modalities will be on separate panels
        #theme_cowplot(font_size = 16, font_family = "Arial") +
        theme_cowplot(font_size = 20, font_family = "Arial") +
        theme(axis.line = element_line(colour = 'black', size = 1),
              axis.ticks = element_line(colour = 'black', size = 1),
              axis.text = element_text(face="bold"),
              axis.line.y = element_line(colour = 'black', size = 0),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              axis.title.y = element_blank(),
              legend.title = element_blank())
    
    
    
    
    # Paste the plots together! 
    # Legends are centered when plots are pasted across groups 
    # e.g. plots have 4 x 1 layout, legends are in column 2 (blind) and 3 (sighted)
    
    
    # SEPARATE PLOTS = LEGENDS NOT NEEDED? 
    
    (v1_line + theme(legend.position= "none")) +
        (vwfa_line + theme(legend.position= "none"))
    
    #IN CASE LEGENDS ARE NEEDED:     
    
    # if(sub_group == "Blind") {
    #     
    #     (v1_line + theme(legend.position= "none")) + 
    #      #   (vwfa_line + theme(legend.position= "bottom")) +
    #         (vwfa_line + theme(legend.position= "none")) +
    #         plot_layout(ncol = 2) 
    #     
    # }else if(sub_group == "Sighted") {
    #     (v1_line + theme(legend.position= "bottom")) + 
    #       #  (vwfa_line + theme(legend.position= "none")) +
    #         (vwfa_line + theme(legend.position= "none")) +
    #         plot_layout(ncol = 2) 
    # }
    
}


### LEXICALITY EFFECTS

#ONLY MAIN EFEFCTS
tms_main_effects_lex <- function(data, sub_group, mod){
    
    #Load color palets
    dark_colors <- brewer.pal(n=3, name = "Dark2") 
    light_colors <- brewer.pal(n=3, name = "Pastel2") 
    
    #LEGEND generated automatically, (re)move it with function call
    # e.g. plot <- beh_reading_speed_points(data = DF) + 
    # theme(legend.position = "none")
    
    data %>% 
        filter(Group == sub_group & Modality == mod) %>% 
        mutate(Lex = factor(Lex, levels = c("word", "pseudo"))) %>% 
        ggplot(aes(x = Lex, y = emmean, color = Modality)) +
        #Crossbar
        stat_summary(fun = "identity", 
                     geom = "crossbar", 
                     size = 1,
                     width = .45) +
        #errorbars
        geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), 
                      width = 0.15, size = 0.8) + 
        scale_color_manual(values = if (sub_group == "Blind") {
                c(dark_colors[1], light_colors[1])}
            else if (sub_group == "Sighted") {
                c(dark_colors[2], light_colors[2])
            }) +
        scale_x_discrete(labels = c("Pseudowords", "Words")) +
        xlab("Lexicality") +
        ylab("Reaction Time [ms]") +
        theme_cowplot(font_size = 20, font_family = "Arial") +
        theme(axis.line = element_line(colour = 'black', size = 1),
              axis.ticks = element_line(colour = 'black', size = 1),
              axis.text = element_text(face="bold"),
              legend.position = "none",
              legend.title = element_blank())
}

tms_line_mod_sep_raw_lex <- function (data, sub_group, metric, mod) {
    
    # Prepare variables with labels and colors
    dark_colors <- brewer.pal(n=3, name = "Dark2") 
    light_colors <- brewer.pal(n=3, name = "Pastel2") 
    
    
    #Legend labels: here group and sensory modalities
    legend_labels <- if(sub_group == "Blind"){
        c("Words (Blind)", "Pseudowords (Blind)")
    } else if (sub_group == "Sighted"){
        c("Words (Sighted)", "Pseudowords (Sighted)")}
    
    # Axis labels, y axis depends on the chosen metric, x describes time windows
    tms_y_label <- if(metric == "RT"){
        "Reaction Time TMS Effect [ms]"}
    else if (metric == "Accuracy"){
        "Accuracy rates TMS Effect [%]"}
    
    tms_x_labels <- c("60-100 ms", "160-200 ms", "260-300 ms")
    
    ## BASIC LINE PLOTS - DOUBLE MODALITIES
    v1_line <- data %>%
        filter(Group == sub_group & Site == "EVC" & Modality == mod) %>%
        mutate(Lex = factor(Lex, levels = c("word", "pseudo"))) %>% 
        mutate(TW = factor(TW, levels = c("earlyTMS", "midTMS", "lateTMS"))) %>% 
        ggplot(aes(x = TW, y = mean, group = Lex)) +
        geom_line(aes(colour = Lex, linetype = Lex), 
                  position = position_dodge(0.2), 
                  size = 1) +
        geom_point(aes(colour = Lex, shape = Lex), 
                   position = position_dodge(0.2), 
                   size = 4) +
        geom_errorbar(aes(ymin = mean-sem, ymax = mean+sem), 
                      position=position_dodge(0.2), width = 0.2, size = 1, 
                      color = if(sub_group == "Blind") {
                          rep(dark_colors[1],6)
                      } else if(sub_group == "Sighted") {
                          rep(dark_colors[2],6)
                      }) +
        geom_hline(yintercept=0, linetype='dashed', col = 'black',size = 1) +
        ylab(tms_y_label) +
        xlab("Time Window") +
        scale_y_continuous(limits = 
                               if(sub_group == "Blind" & metric == "RT" & mod == "read"){
                                    c(-75, 350)}
                                else if(sub_group == "Blind" & metric == "RT" & mod == "speech"){
                                    c(-100, 100)}
                                else if(sub_group == "Sighted" & metric == "RT" & mod == "read"){
                                    c(-20, 100)}
                                else if(sub_group == "Sighted" & metric == "RT" & mod == "speech"){
                                    c(-100, 100)}
                                else if(metric == "Accuracy"){
                                    c(-12, 12)}) +
                                    #c(-2, 2)}) +
        scale_x_discrete(labels = tms_x_labels) +
        scale_color_manual(values = if(sub_group == "Blind") {
            rep(dark_colors[1],2)
        } else if(sub_group == "Sighted") {
            rep(dark_colors[2],2)},
        labels = legend_labels) +
        scale_linetype_manual(values=c("solid", "dashed"), labels = legend_labels) +
        scale_shape(labels = legend_labels) +
        #theme_cowplot(font_size = 16, font_family = "Arial") +
        theme_cowplot(font_size = 20, font_family = "Arial") +
            theme(axis.line = element_line(colour = 'black', size = 1),
                  axis.ticks = element_line(colour = 'black', size = 1),
                  axis.text = element_text(face="bold"),
                  legend.title = element_blank())
    
    
    #Prepare basic "trimmed" vwfa (right side of the plot)
    vwfa_line <- data %>%
        filter(Group == sub_group & Site == "VWFA" & Modality == mod) %>%
        mutate(Lex = factor(Lex, levels = c("word", "pseudo"))) %>% 
        mutate(TW = factor(TW, levels = c("earlyTMS", "midTMS", "lateTMS"))) %>% 
        ggplot(aes(x = TW, y = mean, group = Lex)) +
        geom_line(aes(colour = Lex, linetype = Lex), 
                  position = position_dodge(0.2), 
                  size = 1) +
        geom_point(aes(colour = Lex, shape = Lex), 
                   position = position_dodge(0.2), 
                   size = 4) +
        geom_errorbar(aes(ymin = mean-sem, ymax = mean+sem), 
                      position=position_dodge(0.2), width = 0.2, size = 1, 
                      color = if(sub_group == "Blind") {
                          rep(dark_colors[1],6)
                      } else if(sub_group == "Sighted") {
                          rep(dark_colors[2],6)
                      }) +
        geom_hline(yintercept=0, linetype='dashed', col = 'black',size = 1) +
        ylab(tms_y_label) +
        xlab("Time Window") +
        scale_y_continuous(limits = if(sub_group == "Blind" & metric == "RT" & mod == "read"){
            c(-70, 350)}
            else if(sub_group == "Sighted" & metric == "RT" & mod == "read"){
                c(-20, 100)
            }
            else if(metric == "RT" & mod == "speech"){
                c(-100, 100)
            }
            else if(metric == "Accuracy"){
                 c(-12, 12)}) +
                #c(-2, 2)}) +
        scale_x_discrete(labels = tms_x_labels) +
        scale_color_manual(values = if(sub_group == "Blind") {
            rep(dark_colors[1],2)
        } else if(sub_group == "Sighted") {
            rep(dark_colors[2],2)},
        labels = legend_labels) +
        scale_linetype_manual(values=c("solid", "dashed"), labels = legend_labels) +
        scale_shape(labels = legend_labels) +
        #theme_cowplot(font_size = 16, font_family = "Arial") +
        theme_cowplot(font_size = 20, font_family = "Arial") +
        theme(axis.line = element_line(colour = 'black', size = 1),
              axis.ticks = element_line(colour = 'black', size = 1),
              axis.text = element_text(face="bold"),
              axis.line.y = element_line(colour = 'black', size = 0),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              axis.title.y = element_blank(),
              legend.title = element_blank())
    
    
    
    
    # Paste the plots together! 
    # Legends are centered when plots are pasted across groups 
    # Legends will only be added to the bottom panel (SPEECH)
    # e.g. plots have 4 x 1 layout, legends are in column 2 (blind) and 3 (sighted)
    
    if(sub_group == "Blind" & mod == "read") {
        (v1_line + theme(legend.position= "none")) + 
            (vwfa_line + theme(legend.position= "none")) +
            plot_layout(ncol = 2) 
        
    }else if(sub_group == "Sighted"& mod == "read") {
        (v1_line + theme(legend.position= "none")) + 
            (vwfa_line + theme(legend.position= "none")) +
            plot_layout(ncol = 2) 
        
    }else if(sub_group == "Blind"& mod == "speech") {
        (v1_line + theme(legend.position= "none")) + 
            (vwfa_line + theme(legend.position= "bottom")) +
            plot_layout(ncol = 2)
        
    }else if(sub_group == "Sighted"& mod == "speech") {
        (v1_line + theme(legend.position= "bottom")) + 
            (vwfa_line + theme(legend.position= "none")) +
            plot_layout(ncol = 2) 
    }
    
}

tms_line_mod_sep_glm_lex <- function (data, sub_group, metric, mod) {
    
    # Prepare variables with labels and colors
    dark_colors <- brewer.pal(n=3, name = "Dark2") 
    light_colors <- brewer.pal(n=3, name = "Pastel2") 
    
    
    #Legend labels: here group and sensory modalities
    legend_labels <- if(sub_group == "Blind"){
        c("Words (Blind)", "Pseudowords (Blind)")
    } else if (sub_group == "Sighted"){
        c("Words (Sighted)", "Pseudowords (Sighted)")}
    
    # Axis labels, y axis depends on the chosen metric, x describes time windows
    tms_y_label <- if(metric == "RT"){
        "Reaction Time TMS Effect [ms]"}
    else if (metric == "Accuracy"){
        "Accuracy rate TMS Effect [logit]"}
    
    tms_x_labels <- c("60-100 ms", "160-200 ms", "260-300 ms")
    
    ## BASIC LINE PLOTS - DOUBLE MODALITIES
    v1_line <- data %>%
        filter(Group == sub_group & Site == "EVC" & Modality == mod) %>%
        mutate(Lex = factor(Lex, levels = c("word", "pseudo"))) %>% 
        mutate(TW = factor(TW, levels = c("earlyTMS", "midTMS", "lateTMS"))) %>% 
        ggplot(aes(x = TW, y = estimate, group = Lex)) +
            geom_line(aes(colour = Lex, linetype = Lex), 
                      position = position_dodge(0.2), 
                      size = 1) +
            geom_point(aes(colour = Lex, shape = Lex), 
                       position = position_dodge(0.2), 
                       size = 4) +
            geom_errorbar(aes(ymin = estimate-SE, ymax = estimate+SE), 
                          position=position_dodge(0.2), width = 0.2, size = 1, 
                          color = if(sub_group == "Blind") {
                              rep(dark_colors[1],6)
                          } else if(sub_group == "Sighted") {
                              rep(dark_colors[2],6)
                          }) +
            geom_hline(yintercept=0, linetype='dashed', col = 'black',size = 1) +
            ylab(tms_y_label) +
            xlab("Time Window") +
            scale_y_continuous(limits = 
                                   if(sub_group == "Blind" & metric == "RT" & mod == "read"){
                                       c(-75, 350)}
                               else if(sub_group == "Blind" & metric == "RT" & mod == "speech"){
                                   c(-100, 100)}
                               else if(sub_group == "Sighted" & metric == "RT" & mod == "read"){
                                   c(-20, 100)}
                               else if(sub_group == "Sighted" & metric == "RT" & mod == "speech"){
                                   c(-100, 100)}
                               else if(metric == "Accuracy"){
                                   # c(-12, 12)}) +
                                   c(-2, 3.5)}) +
            scale_x_discrete(labels = tms_x_labels) +
            scale_color_manual(values = if(sub_group == "Blind") {
                rep(dark_colors[1],2)
            } else if(sub_group == "Sighted") {
                rep(dark_colors[2],2)},
            labels = legend_labels) +
            scale_linetype_manual(values=c("solid", "dashed"), labels = legend_labels) +
            scale_shape(labels = legend_labels) +
            #theme_cowplot(font_size = 16, font_family = "Arial") +
            theme_cowplot(font_size = 20, font_family = "Arial") +
            theme(axis.line = element_line(colour = 'black', size = 1),
                  axis.ticks = element_line(colour = 'black', size = 1),
                  axis.text = element_text(face="bold"),
                  legend.title = element_blank())
    
    
    #Prepare basic "trimmed" vwfa (right side of the plot)
    vwfa_line <- data %>%
        filter(Group == sub_group & Site == "VWFA" & Modality == mod) %>%
        mutate(Lex = factor(Lex, levels = c("word", "pseudo"))) %>% 
        mutate(TW = factor(TW, levels = c("earlyTMS", "midTMS", "lateTMS"))) %>% 
        ggplot(aes(x = TW, y = estimate, group = Lex)) +
            geom_line(aes(colour = Lex, linetype = Lex), 
                      position = position_dodge(0.2), 
                      size = 1) +
            geom_point(aes(colour = Lex, shape = Lex), 
                       position = position_dodge(0.2), 
                       size = 4) +
            geom_errorbar(aes(ymin = estimate-SE, ymax = estimate+SE), 
                          position=position_dodge(0.2), width = 0.2, size = 1, 
                          color = if(sub_group == "Blind") {
                              rep(dark_colors[1],6)
                          } else if(sub_group == "Sighted") {
                              rep(dark_colors[2],6)
                          }) +
            geom_hline(yintercept=0, linetype='dashed', col = 'black',size = 1) +
            ylab(tms_y_label) +
            xlab("Time Window") +
            scale_y_continuous(limits = if(sub_group == "Blind" & metric == "RT" & mod == "read"){
                c(-70, 350)}
                else if(sub_group == "Sighted" & metric == "RT" & mod == "read"){
                    c(-20, 100)
                }
                else if(metric == "RT" & mod == "speech"){
                    c(-100, 100)
                }
                else if(metric == "Accuracy"){
                    # c(-12, 12)}) +
                    c(-2, 3.5)}) +
            scale_x_discrete(labels = tms_x_labels) +
            scale_color_manual(values = if(sub_group == "Blind") {
                rep(dark_colors[1],2)
            } else if(sub_group == "Sighted") {
                rep(dark_colors[2],2)},
            labels = legend_labels) +
            scale_linetype_manual(values=c("solid", "dashed"), labels = legend_labels) +
            scale_shape(labels = legend_labels) +
            #theme_cowplot(font_size = 16, font_family = "Arial") +
            theme_cowplot(font_size = 20, font_family = "Arial") +
            theme(axis.line = element_line(colour = 'black', size = 1),
                  axis.ticks = element_line(colour = 'black', size = 1),
                  axis.text = element_text(face="bold"),
                  axis.line.y = element_line(colour = 'black', size = 0),
                  axis.text.y=element_blank(),
                  axis.ticks.y=element_blank(),
                  axis.title.y = element_blank(),
                  legend.title = element_blank())
    
    
    
    
    # Paste the plots together! 
    # Legends are centered when plots are pasted across groups 
    # Legends will only be added to the bottom panel (SPEECH)
    # e.g. plots have 4 x 1 layout, legends are in column 2 (blind) and 3 (sighted)
    
    if(sub_group == "Blind" & mod == "read") {
        (v1_line + theme(legend.position= "none")) + 
            (vwfa_line + theme(legend.position= "none")) +
            plot_layout(ncol = 2) 
        
    }else if(sub_group == "Sighted"& mod == "read") {
        (v1_line + theme(legend.position= "none")) + 
            (vwfa_line + theme(legend.position= "none")) +
            plot_layout(ncol = 2) 
        
    }else if(sub_group == "Blind"& mod == "speech") {
        (v1_line + theme(legend.position= "none")) + 
            (vwfa_line + theme(legend.position= "bottom")) +
            plot_layout(ncol = 2)
        
    }else if(sub_group == "Sighted"& mod == "speech") {
        (v1_line + theme(legend.position= "bottom")) + 
            (vwfa_line + theme(legend.position= "none")) +
            plot_layout(ncol = 2) 
    }
    
    
}

