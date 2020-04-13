# Training loop for machine learning using caret

# Individual model training loop ----

# Load in required libraries
library(dplyr)
library(tidyr)
library(caret)

# Define training control scheme
fitControl <- trainControl(method = "repeatedCV",
                           number = 10,
                           repeats = 3,
                           classProbs = TRUE,
                           summaryFunction = prSummary,
                           verboseIter = TRUE)

# Construct training function
custom_training_loop <- function(mod_type, all_data, trnCtrl, smpl){
# Inputs:
#   mod_type: Model type as string (must correspond to methods in caret)
#   all_data: Total cleaned dataset as dataframe
#   ense_ctrl: trainControl object for model training
#   smpl: String indicating resampling method ('up', 'down', 'smote'). Will
#         not resample if any other input is given (inc. no input).
#
# Returns:
#   trained_model_output: List of resampled (or not) data, testing data, type
#                         of resampling, trained model, predictions of testing
#                         set, and a confusion matrix
  
  # Initialize return object
  trained_model_output <- c()
  
  # Create data partitions
  inTraining <- createDataPartitions(all_data$master_sti,
                                     p = 0.75,
                                     list = FALSE)
  all_data$master_sti <- as.factor(all_data$master_sti)
  levels(all_data$master_sti) <- c('No', 'Yes')
  
  # Split into training and testing
  training <- all_data[ inTraining,]
  testing  <- all_data[-inTraining,]
  
  if (smpl == 'smote'){
    trained_model_output$resample_type <- 'smote'
    training$master_sti <- as.factor(training$master_sti)
    smote_train <- SMOTE(master_sti ~ ., data = training)
    smote_train$master_sti <- as.factor(smote_train$master_sti)
    training <- smote_train
  } else if (smpl == 'up') {
    trained_model_output$resample_type <- 'upsampling'
    up_train <- upSample(x = training[, -ncol(training)],
                         y = training$master_sti)
    colnames(up_train) <- colnames(training)
    training <- up_train
  } else if (smpl == 'down') {
    trained_model_output$resample_type <- 'downsampling'
    down_train <- downSample(x = training[, -ncol(training)],
                         y = training$master_sti)
    colnames(down_train) <- colnames(training)
    training <- down_train
  } else {
    training <- training
  }
  
  # Create predictions from trained model
  preds <- predict(trained_model_output$model, testing)
  
  # Add results to output object
  trained_model_output$testing_preds <- preds
  trained_model_output$confMat <- confusionMatrix(preds,
                                                  testing$master_sti,
                                                  mode = 'preds_recall',
                                                  positive = 'Yes')
  trained_model_output$testing_data <- testing
  trained_model_output$training_data <- training
  
  return(trained_model_output)
}

# Ensemble Training Function ----
# Load required libraries
library(caretEnsemble)
library(caTools)

# Construct Control for Ensemble Model
ensControl <- trainControl(method = "repeatedCV",
                           number = 10,
                           repeats = 3,
                           classProbs = TRUE,
                           summaryFunction = prSummary,
                           savePredictions = 'final',
                           verboseIter = TRUE)

ensemble_train_function <-function(model_list, all_data, ense_ctrl, smpl){
# Inputs:
#   model_list: List of models as strings (must correspond to methods in caret)
#   all_data: Total cleaned dataset as dataframe
#   ense_ctrl: trainControl object for ensemble training
#   smpl: String indicating resampling method ('up', 'down', 'smote'). Will
#         not resample if any other input is given (inc. no input).
#
# Returns:
#   trained_model_output: List of resampled (or not) data, testing data, type
#                         of resampling, trained model, predictions of testing
#                         set, and a confusion matrix
  
  # Create return variable
  trained_model_output <- c()
  
  # Create data partitions
  inTraining <- createDataPartition(all_data$master_sti, p=.75, list=FALSE)
  all_data$master_sti <- as.factor(all_data$master_sti)
  levels(all_data$master_sti) <- c("No", "Yes")
  
  # Split into training and testing
  training <- all_data[ inTraining, ]
  testing  <- all_data[-intraining, ]
  
  if (smpl == 'smote'){
    # Use SMOTE
    trained_model_output$resample_type <- 'smote'
    training$master_sti <- as.factor(training$master_sti)
    smote_train <- SMOTE(master_sti ~ ., data = training)
    smote_train$master_sti <- as.factor(smote_train$master_sti)
    training <- smote_train
  } else if (smpl == 'up'){
    # Upsampling
    trained_model_output$resample_type <- 'upsampling'
    up_train <- upSample(x = training[, -ncol(training)],
                         y = training$master_sti)
    colnames(up_train) <- colnames(training)
    training <- up_train
  } else if (smpl == 'down'){
    # Downsampling
    trained_model_output$resample_type <- 'downsampling'
    down_train <- downSample(x = training[, -ncol(training)],
                             y = training$master_sti)
    colnames(down_train) <- colnames(training)
    training <- down_train
  } else{
    # Just pass train to train, unedited
    training <- training
  }
  
  # Model code
  caret_list <- caretList(master_sti ~.,
                          data = training,
                          trControl = ense_ctrl,
                          metric = 'AUC',
                          methodList = model_list)
  
  model_ensemble <- caretEnsemble(caret_list,
                                  metric = 'AUC',
                                  trControl = trainControl(
                                    number = 2,
                                    summaryFunction = prSummary,
                                    classProbs = TRUE
                                  ))
  
  # Output
  trained_model_output$ensemble <- model_ensemble
  trained_model_output$testing_preds <- predict(model_ensemble, testing)
  trained_model_output$confMat <- confusionMatrix(trained_model_output$testing_preds,
                                                  testing$master_sti,
                                                  mode = 'prec_recall',
                                                  positive = 'Yes')
  trained_model_output$testing_data  <- testing
  trained_model_output$training_data <- training
  
  # Finally, return everything as the output variable
  return(trained_model_output)
}