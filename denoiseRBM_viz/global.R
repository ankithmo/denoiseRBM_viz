
###############################################################################
# Libraries
###############################################################################
library(shiny)
library(shinydashboard)
library(plotly)
library(tidyverse)
library(glue)
###############################################################################
# Pre-computations
###############################################################################
get_dataset_path <- function(dataset){
  file.path("C:", "Users", "Ankith", "Desktop", "thesis", "denoiseRBM_viz", 
            "results", dataset)
}

get_MLP_files <- function(path){
  # X_c, A_c
  X_c_A_c_0_val <- read.csv(file.path(path, "X_c", "A_c", "val", 
                                      "X_c_A_c_0_val.csv"), sep = "\t")
  X_c_A_c_0_test <- read.csv(file.path(path, "X_c", "A_c", "test",
                                      "X_c_A_c_0_test.csv"), sep = "\t")
  # X_z, A_c
  X_z_A_c_0_val <- read.csv(file.path(path, "X_z", "A_c", "val",
                                      "X_z_A_c_0_val.csv"), sep = "\t")
  X_z_A_c_0_test <- read.csv(file.path(path, "X_z", "A_c", "test",
                                        "X_z_A_c_0_test.csv"), sep = "\t")  
  return_list <- list("X_c_A_c_val" = X_c_A_c_0_val, 
                      "X_c_A_c_test" = X_c_A_c_0_test, 
                      "X_z_A_c_val" = X_z_A_c_0_val, 
                      "X_z_A_c_test" = X_z_A_c_0_test)
  return(return_list)
}

get_DNN_files <- function(path){
  
  for(p in seq(0,100,10)){
    # X_c, A_c
    if (p == 0){
      X_c_A_c_val <- read.csv(file.path(path, "X_c", "A_c", "val", 
                                        glue("X_c_A_c_0_val.csv")), sep = "\t")
    } else {
      X_c_A_c_val <- rbind(X_c_A_c_val, read.csv(file.path(path, "X_c", "A_c", "val", 
                                                           glue("X_c_A_c_{p}_val.csv")), sep = "\t"))
    }
    if (p == 0){
      X_c_A_c_test <- read.csv(file.path(path, "X_c", "A_c", "test", 
                                         glue("X_c_A_c_0_test.csv")), sep = "\t")
    } else {
      X_c_A_c_test <- rbind(X_c_A_c_test, read.csv(file.path(path, "X_c", "A_c", "test", 
                                                  glue("X_c_A_c_{p}_test.csv")), sep = "\t"))
    }
    
    # X_c, A_z
    if (p == 0){
      X_c_A_z_val <- read.csv(file.path(path, "X_c", "A_z", "val", 
                                        glue("X_c_A_z_0_val.csv")), sep = "\t")
    } else {
      if (p <= 40){
        X_c_A_z_val <- rbind(X_c_A_z_val, read.csv(file.path(path, "X_c", "A_z", "val", 
                                                             glue("X_c_A_z_{p}_val.csv")), sep = "\t"))
      } else {
        if (grepl("n2v", path, fixed=T) == F){
          X_c_A_z_val <- rbind(X_c_A_z_val, read.csv(file.path(path, "X_c", "A_z", "val", 
                                                      glue("X_c_A_z_{p}_val.csv")), sep = "\t"))
        }
      }
    }
    if (p == 0){
      X_c_A_z_test <- read.csv(file.path(path, "X_c", "A_z", "test", 
                                          glue("X_c_A_z_0_test.csv")), sep = "\t")
    } else {
      if (p <= 40){
        X_c_A_z_test <- rbind(X_c_A_z_test, read.csv(file.path(path, "X_c", "A_z", "test", 
                                                               glue("X_c_A_z_{p}_test.csv")), sep = "\t"))
      } else {
        if ((grepl("n2v", path, fixed=T)) == F){
          X_c_A_z_test <- rbind(X_c_A_z_test, read.csv(file.path(path, "X_c", "A_z", "test", 
                                                      glue("X_c_A_z_{p}_test.csv")), sep = "\t"))
        }
      }
    }
    
    # X_z, A_c
    if (p == 0){
      X_z_A_c_val <- read.csv(file.path(path, "X_z", "A_c", "val", 
                                        glue("X_z_A_c_0_val.csv")), sep = "\t")
    } else {
      X_z_A_c_val <- rbind(X_z_A_c_val, read.csv(file.path(path, "X_z", "A_c", "val", 
                                                  glue("X_z_A_c_{p}_val.csv")), sep = "\t"))
    }
    if (p == 0){
      X_z_A_c_test <- read.csv(file.path(path, "X_z", "A_c", "test", 
                                        glue("X_z_A_c_0_test.csv")), sep = "\t")
    } else {
      X_z_A_c_test <- rbind(X_z_A_c_test, read.csv(file.path(path, "X_z", "A_c", "test", 
                                                glue("X_z_A_c_{p}_test.csv")), sep = "\t"))
    }
    
    # X_z, A_z
    if (p == 0){
      X_z_A_z_val <- read.csv(file.path(path, "X_z", "A_z", "val", 
                                        glue("X_z_A_z_0_val.csv")), sep = "\t")
    } else {
      if (p <= 40){
        X_z_A_z_val <- rbind(X_z_A_z_val, read.csv(file.path(path, "X_z", "A_z", "val", 
                                                             glue("X_z_A_z_{p}_val.csv")), sep = "\t"))
      } else {
        if ((grepl("n2v", path, fixed=T)) == F){
          X_z_A_z_val <- rbind(X_z_A_z_val, read.csv(file.path(path, "X_z", "A_z", "val", 
                                                             glue("X_z_A_z_{p}_val.csv")), sep = "\t"))
        }
      }
    }
    if (p == 0){
      X_z_A_z_test <- read.csv(file.path(path, "X_z", "A_z", "test", 
                                         glue("X_z_A_z_0_test.csv")), sep = "\t")
    } else {
      if (p <= 40){
        X_z_A_z_test <- rbind(X_z_A_z_test, read.csv(file.path(path, "X_z", "A_z", "test", 
                                                               glue("X_z_A_z_{p}_test.csv")), sep = "\t"))
      } else {
        if ((grepl("n2v", path, fixed=T)) == F){
          X_z_A_z_test <- rbind(X_z_A_z_test, read.csv(file.path(path, "X_z", "A_z", "test", 
                                                               glue("X_z_A_z_{p}_test.csv")), sep = "\t"))
        }
      }
    }
  }
  return_list <- list("X_c_A_c_val" = X_c_A_c_val, 
                      "X_c_A_c_test" = X_c_A_c_test, 
                      "X_c_A_z_val" = X_c_A_z_val, 
                      "X_c_A_z_test" = X_c_A_z_test,
                      "X_z_A_c_val" = X_z_A_c_val, 
                      "X_z_A_c_test" = X_z_A_c_test,
                      "X_z_A_z_val" = X_z_A_z_val, 
                      "X_z_A_z_test" = X_z_A_z_test)
  return(return_list)
}

###############################################################################
# Datasets for WikiCS
###############################################################################
# w_dataset <- "WikiCS"
# w_dataset_path <- get_dataset_path(w_dataset)
# 
# w_MLP_path <- file.path(w_dataset_path, "MLP")
# w_MLP_files <- get_MLP_files(w_MLP_path)
# 
# w_n2v_path <- file.path(w_dataset_path, "n2v")
# w_n2v_files <- get_DNN_files(w_n2v_path)
# 
# w_GCN_path <- file.path(w_dataset_path, "GCN")
# w_GCN_files <- get_DNN_files(w_GCN_path)
# 
# w_SAGE_path <- file.path(w_dataset_path, "SAGE")
# w_SAGE_files <- get_DNN_files(w_SAGE_path)
###############################################################################
# Datasets for WikiCS_AWGN
###############################################################################
# wa_dataset <- "WikiCS-AWGN"
# wa_dataset_path <- get_dataset_path(wa_dataset)
# 
# wa_MLP_path <- file.path(wa_dataset_path, "MLP")
# wa_MLP_files <- get_MLP_files(wa_MLP_path)
# 
# wa_n2v_path <- file.path(wa_dataset_path, "n2v")
# wa_n2v_files <- get_DNN_files(wa_n2v_path)
# 
# wa_GCN_path <- file.path(wa_dataset_path, "GCN")
# wa_GCN_files <- get_DNN_files(wa_GCN_path)
# 
# wa_SAGE_path <- file.path(wa_dataset_path, "SAGE")
# wa_SAGE_files <- get_DNN_files(wa_SAGE_path)
###############################################################################
# Datasets for ogbn-arxiv
###############################################################################
o_dataset <- "ogbn-arxiv"
o_dataset_path <- get_dataset_path(o_dataset)

o_MLP_path <- file.path(o_dataset_path, "MLP")
o_MLP_files <- get_MLP_files(o_MLP_path)

o_n2v_path <- file.path(o_dataset_path, "n2v")
o_n2v_files <- get_DNN_files(o_n2v_path)

o_GCN_path <- file.path(o_dataset_path, "GCN")
o_GCN_files <- get_DNN_files(o_GCN_path)

o_SAGE_path <- file.path(o_dataset_path, "SAGE")
o_SAGE_files <- get_DNN_files(o_SAGE_path)
###############################################################################
steps <- read_csv2("help.csv")
###############################################################################