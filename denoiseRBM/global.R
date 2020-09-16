
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
  file.path("results", dataset)
}

get_MLP_files <- function(path){
  #############################################################################
  # X_c, A_c
  #############################################################################
  # val
  X_c_A_c_val_file <- file.path(path, "X_c", "A_c", "val", "X_c_A_c_0_val.csv")
  X_c_A_c_val <- if (file.exists(X_c_A_c_val_file)) read.csv(X_c_A_c_val_file) else NULL

  # test
  X_c_A_c_test_file <- file.path(path, "X_c", "A_c", "test", "X_c_A_c_0_test.csv")
  X_c_A_c_test <- if (file.exists(X_c_A_c_test_file)) read.csv(X_c_A_c_test_file) else NULL
  #############################################################################
  # X_z, A_c
  #############################################################################
  # val
  X_z_A_c_val_file <- file.path(path, "X_z", "A_c", "val", "X_z_A_c_0_val.csv")
  X_z_A_c_val <- if (file.exists(X_z_A_c_val_file)) read.csv(X_z_A_c_val_file) else NULL
  
  # test
  X_z_A_c_test_file <- file.path(path, "X_z", "A_c", "test", "X_z_A_c_0_test.csv")
  X_z_A_c_test <- if (file.exists(X_z_A_c_test_file)) read.csv(X_z_A_c_test_file) else NULL
  #############################################################################
  return_list <- list("X_c_A_c_val" = X_c_A_c_val, 
                      "X_c_A_c_test" = X_c_A_c_test, 
                      "X_z_A_c_val" = X_z_A_c_val, 
                      "X_z_A_c_test" = X_z_A_c_test)
  return(return_list)
}

get_DNN_files <- function(path){
  
  for(p in seq(0,100,10)){
    ###########################################################################
    # X_c, A_c
    ###########################################################################
    # val
    X_c_A_c_val_exist <- exists("X_c_A_c_val")
    X_c_A_c_val_file <- file.path(path, "X_c", "A_c", "val", glue("X_c_A_c_{p}_val.csv"))
    if (file.exists(X_c_A_c_val_file)){
      if (X_c_A_c_val_exist == F){
        X_c_A_c_val <- read.csv(X_c_A_c_val_file)
      } else {
        X_c_A_c_val <- rbind(X_c_A_c_val, read.csv(X_c_A_c_val_file))  
      }
    }
    
    # test
    X_c_A_c_test_exist <- exists("X_c_A_c_test")
    X_c_A_c_test_file <- file.path(path, "X_c", "A_c", "test", glue("X_c_A_c_{p}_test.csv"))
    if (file.exists(X_c_A_c_test_file)){
      if (X_c_A_c_test_exist == F){
        X_c_A_c_test <- read.csv(X_c_A_c_test_file)
      } else {
        X_c_A_c_test <- rbind(X_c_A_c_test, read.csv(X_c_A_c_test_file))  
      }
    }
    ###########################################################################
    # X_c, A_z
    ###########################################################################
    # val
    X_c_A_z_val_exist <- exists("X_c_A_z_val")
    X_c_A_z_val_file <- file.path(path, "X_c", "A_z", "val", glue("X_c_A_z_{p}_val.csv"))
    X_c_A_z_val_flag <- F
    if(grepl("n2v", path, fixed=T) == T && p > 40){
      X_c_A_z_val_flag <- T 
    }
    if (file.exists(X_c_A_z_val_file)){
      if (X_c_A_z_val_flag == F){
        if (X_c_A_z_val_exist == F){
          X_c_A_z_val <- read.csv(X_c_A_z_val_file)
        } else {
          X_c_A_z_val <- rbind(X_c_A_z_val, read.csv(X_c_A_z_val_file))
        }
      }
    }
    
    # test
    X_c_A_z_test_exist <- exists("X_c_A_z_test")
    X_c_A_z_test_file <- file.path(path, "X_c", "A_z", "test", glue("X_c_A_z_{p}_test.csv"))
    X_c_A_z_test_flag <- F
    if(grepl("n2v", path, fixed=T) == T && p > 40){
      X_c_A_z_test_flag <- T 
    }
    if (file.exists(X_c_A_z_test_file)){
      if (X_c_A_z_test_flag == F){
        if (X_c_A_z_test_exist == F){
          X_c_A_z_test <- read.csv(X_c_A_z_test_file)
        } else {
          X_c_A_z_test <- rbind(X_c_A_z_test, read.csv(X_c_A_z_test_file))  
        }
      }
    }
    ###########################################################################
    # X_z, A_c
    ###########################################################################
    # val
    X_z_A_c_val_exist <- exists("X_z_A_c_val")
    X_z_A_c_val_file <- file.path(path, "X_z", "A_c", "val", glue("X_z_A_c_{p}_val.csv"))
    if (file.exists(X_z_A_c_val_file)){
      if (X_z_A_c_val_exist == F){
        X_z_A_c_val <- read.csv(X_z_A_c_val_file)
      } else {
        X_z_A_c_val <- rbind(X_z_A_c_val, read.csv(X_z_A_c_val_file))  
      }
    }
    
    # test
    X_z_A_c_test_exist <- exists("X_z_A_c_test")
    X_z_A_c_test_file <- file.path(path, "X_z", "A_c", "test", glue("X_z_A_c_{p}_test.csv"))
    if (file.exists(X_z_A_c_test_file)){
      if (X_z_A_c_test_exist == F){
        X_z_A_c_test <- read.csv(X_z_A_c_test_file)
      } else {
        X_z_A_c_test <- rbind(X_z_A_c_test, read.csv(X_z_A_c_test_file))  
      }
    }
    ###########################################################################
    # X_z, A_z
    ###########################################################################
    # val
    X_z_A_z_val_exist <- exists("X_z_A_z_val")
    X_z_A_z_val_file <- file.path(path, "X_z", "A_z", "val", glue("X_z_A_z_{p}_val.csv"))
    X_z_A_z_val_flag <- F
    if(grepl("n2v", path, fixed=T) == T && p > 40){
      X_z_A_z_val_flag <- T 
    }
    if (file.exists(X_z_A_z_val_file)){
      if (X_z_A_z_val_flag == F){
        if (X_z_A_z_val_exist == F){
          X_z_A_z_val <- read.csv(X_z_A_z_val_file)
        } else {
          X_z_A_z_val <- rbind(X_z_A_z_val, read.csv(X_z_A_z_val_file))
        }
      }
    }
    
    # test
    X_z_A_z_test_exist <- exists("X_z_A_z_test")
    X_z_A_z_test_file <- file.path(path, "X_z", "A_z", "test", glue("X_z_A_z_{p}_test.csv"))
    X_z_A_z_test_flag <- F
    if(grepl("n2v", path, fixed=T) == T && p > 40){
      X_z_A_z_test_flag <- T 
    }
    if (file.exists(X_z_A_z_test_file)){
      if (X_z_A_z_test_flag == F){
        if (X_z_A_z_test_exist == F){
          X_z_A_z_test <- read.csv(X_z_A_z_test_file)
        } else {
          X_z_A_z_test <- rbind(X_z_A_z_test, read.csv(X_z_A_z_test_file))  
        }
      }
    }
  }
  ###########################################################################
  X_c_A_c_val <- if (X_c_A_c_val_exist == T) X_c_A_c_val else NULL
  X_c_A_c_test <- if (X_c_A_c_test_exist == T) X_c_A_c_test else NULL
  X_c_A_z_val <- if (X_c_A_z_val_exist == T) X_c_A_z_val else NULL
  X_c_A_z_test <- if (X_c_A_z_test_exist == T) X_c_A_z_test else NULL
  X_z_A_c_val <- if (X_z_A_c_val_exist == T) X_z_A_c_val else NULL
  X_z_A_c_test <- if (X_z_A_c_test_exist == T) X_z_A_c_test else NULL
  X_z_A_z_val <- if (X_z_A_z_val_exist == T) X_z_A_z_val else NULL
  X_z_A_z_test <- if (X_z_A_z_test_exist == T) X_z_A_z_test else NULL
  ###########################################################################
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
w_dataset <- "WikiCS"
w_dataset_path <- get_dataset_path(w_dataset)

w_MLP_path <- file.path(w_dataset_path, "MLP")
w_MLP_files <- get_MLP_files(w_MLP_path)

w_n2v_path <- file.path(w_dataset_path, "n2v")
w_n2v_files <- get_DNN_files(w_n2v_path)

w_GCN_path <- file.path(w_dataset_path, "GCN")
w_GCN_files <- get_DNN_files(w_GCN_path)

w_SAGE_path <- file.path(w_dataset_path, "SAGE")
w_SAGE_files <- get_DNN_files(w_SAGE_path)
###############################################################################
# Datasets for WikiCS_AWGN
###############################################################################
wa_dataset <- "WikiCS-AWGN"
wa_dataset_path <- get_dataset_path(wa_dataset)

wa_MLP_path <- file.path(wa_dataset_path, "MLP")
wa_MLP_files <- get_MLP_files(wa_MLP_path)

wa_n2v_path <- file.path(wa_dataset_path, "n2v")
wa_n2v_files <- get_DNN_files(wa_n2v_path)

wa_GCN_path <- file.path(wa_dataset_path, "GCN")
wa_GCN_files <- get_DNN_files(wa_GCN_path)

wa_SAGE_path <- file.path(wa_dataset_path, "SAGE")
wa_SAGE_files <- get_DNN_files(wa_SAGE_path)
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