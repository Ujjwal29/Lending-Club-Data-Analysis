
drops = c("desc","id","member_id","policy_code","paymnt_plan","title","zip_code","url")
date_drops = c("issue_d","pymnt_plan","last_pymnt_d","next_pymnt_d","last_credit_pull_d","dti_calculated","dti_wrong","next_pymnt_d_invalid_check","earliest_cr_line")
dates_cleaned <- dates_cleaned[,!(names(dates_cleaned) %in% drops)]
dates_cleaned <- dates_cleaned[,!(names(dates_cleaned) %in% date_drops)]

df = dates_cleaned
cols <- colnames(df)
na.test <-  function (x) {
  w <- sapply(x, function(x)any(is.na(x)))
  if (any(w)) {
    stop(paste("All NA in columns", paste(cols[which(w)], collapse=", ")))
  }
}
na.test(df)
sapply(df, function(x)any(is.na(x)))

df$delinq_2yrs[is.na(df$delinq_2yrs)] <- 0
df$inq_last_6mths[is.na(df$inq_last_6mths)] <- 0

open_acc_na <- df[is.na(df$open_acc),]
df_fullPaid <- df[df$loan_status == 'Does not meet the credit policy. Status:Fully Paid',]
df$open_acc[is.na(df$open_acc)] <- median(df_fullPaid$open_acc,na.rm=TRUE)

df$total_acc[is.na(df$total_acc)] <- median(df_fullPaid$total_acc,na.rm=TRUE)

pub_rec_na <- df[is.na(df$pub_rec),]
df$pub_rec[is.na(df$pub_rec)] <- median(df_fullPaid$pub_rec,na.rm=TRUE)
df$pub_rec[df$pub_rec > 10] <- 10

df$revol_util[is.na(df$revol_util)] <- mean(df$revol_util,na.rm=TRUE)
df <- subset(df,revol_util < 100)

df$acc_now_delinq[df$acc_now_delinq > 3] <- 3
df$acc_now_delinq[is.na(df$acc_now_delinq)] <- 3

df$collections_12_mths_ex_med[is.na(df$collections_12_mths_ex_med)] <- 0

df <- subset(df,!is.na(annual_inc))
#pub_rec_na <- df[is.na(df$pub_rec),]
#sapply(df, function(x)any(is.na(x)))