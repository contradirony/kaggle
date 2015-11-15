##########################################################################
# Description: Model using recipe-'cuisine' similarity
# For each cuisine, create an 'ideal' vector so when a new recipe comes in,
# see which type it falls under
#
# 2 versions possible: manual vs random forest
# created: Nov 2015
# @author: samlam
###########################################################################

load("~/kaggle/whats-cooking/whats-cooking/recipe-ingred-mat.RData")
dim(out)
object.size(out)