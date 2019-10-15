ans_tbl <- data.frame(block = letters[1:10], rep = 1:10, ans =  NA)

#single row
this_ans_df <- data.frame("d", 4, 5)  
ins_row <- which(ans_tbl$block == as.character(this_ans_df[1,1]))

ans_tbl[ins_row, ] <- this_ans_df
ans_tbl

# double row
this_ans_df <- data.frame(block = c("g", "h"), rep = 1:2, ans = 1:2)
ins_row <- which(ans_tbl$block == as.character(this_ans_df[1,1]))
ins_nrows <- nrow(this_ans_df) - 1

ans_tbl[ins_row:(ins_row + ins_nrows), ] <- this_ans_df
ans_tbl

#####
this_ans <- NULL

this_ans[4] <- 1
this_ans
this_ans[5]
this_ans
length(this_ans)
this_ans[7] <- NA
this_ans
print("cannot save in order of click, needs predefined str.")
