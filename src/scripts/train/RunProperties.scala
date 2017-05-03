package scripts.train

import java.util.Properties

import scripts.train.RunTrain.runProperties

final class RunProperties extends Properties {
  def jamrRoot = getProperty("jamrRoot")
  def corpusFolder = getProperty("corpus.folder")
  def corpusFileBaseName = getProperty("corpus.file_base_name")
  def modelFolderSuffix = getProperty("model.folder_suffix")

  def skipPreprocessing = getProperty("skip.preprocessing", "false").toBoolean
  def skipConceptTableExtraction = getProperty("skip.concept_table_extract", "false").toBoolean
  def skipTrainStage1 = getProperty("skip.train.stage1", "false").toBoolean
  def skipTrainStage2 = getProperty("skip.train.stage2", "false").toBoolean
  def skipEvaluating = getProperty("skip.evaluate", "false").toBoolean
  def skipEvaluateAllStageDecode = getProperty("skip.evaluate.allstage.decode", "false").toBoolean
  def skipEvaluateStage2Decode = getProperty("skip.evaluate.stage2.decode", "false").toBoolean
}
