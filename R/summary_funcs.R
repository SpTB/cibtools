#' Creates a table summarizing session-level correlations between size ratings and actual item sizes
#'
#' @param ratings data frame containing ratings for
#' @param exp experiment number
#' @param method correlation method (used by the default r cor.test function)
#'
#' @return a summary tibble with correlations between pre and post-task ratings
#' @export
#'
getSizeAcc <- function(ratings, exp=2, method = 'spearman') {

  if(exp==1) {
    # ratings = read_csv('../2_pipeline/augmented all files/ratings.csv')
    ratings$pall = (ratings$ratP1 + ratings$ratP2 + ratings$ratP3)/3

    size_acc = ratings %>%
      dplyr::group_by(sessionTotal) %>%
      dplyr::summarise(size_est = stats::cor(pall, obj_size,method = 'spearman'),
                size_pval = stats::cor.test(pall, obj_size)$p.value, method = method)

  } else {
    size = fread('H:/CB3_code/CB_V1/pics2/Results24.csv')
    ratings$obj_size = rep(size$objectsize, length(unique(ratings$subjID)))
    ratings$pall = (ratings$ratP1_pre + ratings$ratP2_pre + ratings$ratP_post)/3

    size_acc = ratings %>%
      dplyr::filter(!is.na(ratV_post)) %>%
      dplyr::group_by(subjID) %>%
      dplyr::summarise(size_est = stats::cor(pall, obj_size,method = 'spearman'),
                size_pval = stats::cor.test(pall, obj_size)$p.value, method = method)
  }
  #add exp num
  size_acc$exp = rep(exp, nrow(size_acc))

  return (size_acc)
}

#'Creates a table summarizing session-level correlations between pre and post-task ratings

#' @param ratings ratings data frame
#' @param exp experiment number

#' @return a summary tibble of pre and post-task correlations
#' @export
getRatPrePostCor <- function(ratings, exp=2) {

  if(exp==1) {
    # ratings = read_csv('../2_pipeline/augmented all files/ratings.csv')

    tab = ratings %>%
      dplyr::group_by(sessionTotal) %>%
      dplyr::summarise(pref_cor = stats::cor(v1, ratV3),
                size_cor = stats::cor(p1, ratP3),
                pref_pval = stats::cor.test(v1, ratV3)$p.value,
                size_pval = stats::cor.test(p1, ratP3)$p.value)

  } else {

    tab = ratings %>%
      dplyr::group_by(subjID) %>%
      dplyr::filter(!is.na(ratV_post)) %>%
      dplyr::summarise(pref_cor = stats::cor(preV_mean, ratV_post),
                size_cor = stats::cor(preP_mean, ratP_post),
                pref_pval = stats::cor.test(preV_mean, ratV_post)$p.value,
                size_pval = stats::cor.test(preP_mean, ratP_post)$p.value)
  }

  return (tab)
}

#'Creates a table summarizing session-level correlations between ratings and choices

#' @param data main data frame
#' @param exp experiment number
#' @param round_pval p-value rounding (how many decimals)
#'
#' @return a summary tibble of rating-choice consistency %
#' @export
sumRCcons <- function(data, exp=2, round_pval=4) {
  # summarizes rating-choice consistency %
  # exp: experiment number
  # data: main data frame (all[[x]])
  #
  if (exp==1) id1= 'sessionTotal' else id1= 'subjID'

    out = data %>%
      dplyr::filter(dec_type %in% c('per','val')) %>%
      dplyr::group_by_at( vars(id1, dec_type)) %>%
      dplyr::summarise(acc = mean(acc, na.rm=T))%>%
      dplyr::group_by(dec_type) %>%
      dplyr::summarise(consRD_mean = mean(acc),
                consRD_SD = stats::sd(acc),
                t_df = stats::t.test(acc, mu=0.5)$parameter,
                t_score = stats::t.test(acc, mu=0.5)$statistic,
                p_value = round(stats::t.test(acc, mu=0.5)$p.value,4))

  return (out)
}

#'Creates a table summarizing session-level correlations between ratings and judgments

#' @param data main data frame
#' @param exp experiment number
#' @param round_pval p-value rounding (how many decimals)
#'
#' @return a summary tibble of rating-judgment consistency %
#' @export
sumRJcons <- function(data, exp, round_pval=4) {
  if (exp==1) id1= 'sessionTotal' else id1= 'subjID'

  out = data %>%
    dplyr::group_by_at( vars(id1, judge_type)) %>%
    dplyr::summarise(RJ = mean(consRJ, na.rm=T)) %>%
    dplyr::group_by(judge_type) %>%
    dplyr::summarise(consRJ_mean = mean(RJ),
              consRJ_SD = stats::sd(RJ),
              t_df = stats::t.test(RJ, mu=0.5)$parameter,
              t_score = stats::t.test(RJ, mu=0.5)$statistic,
              p_value = round(stats::t.test(RJ, mu=0.5)$p.value,4))

  return (out)
}

## Effects of Choices on Ratings:

#' Creates a rating df containing choice info

#' @param main a list of main dataframes of all online experiments OR a dataframe from a single experiment
#' @param ratings a list of rating dataframes of all online experiments OR a dataframe from a single experiment

#' @return a a rating df with choice counts
#' @export

getRatDiffs <- function (main, ratings) {
  #made to take in a list of dataframes (E2-E5), but should also be able to handle a single df
  out = list()
  #create sequence for iter:
  if (class(main)[1] == 'list') {
    iter_seq = 2:length(main)
    } else {
      iter_seq = 1
      main=list(main)
      ratings=list(ratings)
    }

  for (i in iter_seq) {
    rat_count = main[[i]] %>%
      dplyr::filter(dec_type!='no') %>%
      purrr::modify_if(is.character, as.factor) %>% #all factors for grouping
      dplyr::group_by(subjID, dec_type,chosen) %>%
      dplyr::summarise(count=n()) %>%
      tidyr::complete(chosen, fill=list(count=0)) %>% #complete missing factor levels of chosen
      dplyr::filter(!is.na(chosen)) %>%
      dplyr::rename(pics=chosen) %>% #change for consistency with ratings
      tidyr::pivot_wider(names_from = dec_type, values_from = count) %>%
      dplyr::left_join(ratings[[i]]) %>%
      dplyr::filter(chosen==T) %>% # pics only used for given subject
      #add difference cols
      dplyr::mutate(P3minP1 = ratV_post-ratV1_pre,
             P3minP2 = ratV_post-ratV2_pre,
             P2minP1 = ratV2_pre-ratV1_pre,
             S3minS1 = ratP_post-ratP1_pre,
             S3minS2 = ratP_post-ratP2_pre,
             S2minS1 = ratP2_pre-ratP1_pre) %>%
      dplyr::select(subjID, pics, size_choice_num=per, pref_choice_num=val,
             P3minP1, P3minP2, P2minP1, S3minS1, S3minS2, S2minS1)

    rat_count$expNum = rep(i,nrow(rat_count))

    if (i == 1) out = rat_count else out = bind_rows(out, rat_count)
  }

  out$subjID2 = as.factor(paste(out$expNum, out$subjID))
  return (out)
}

