make_pair <- function() {
  llm <- data.frame(id = 1:10, A = c(TRUE, TRUE, FALSE, FALSE, TRUE, rep(FALSE, 5)))
  human <- data.frame(id = 1:10, A = c(1, 0, 0, 0, 1, 1, 0, 0, 0, 0))
  list(llm = llm, human = human)
}

test_that("code_agreement matches hand-computed kappa, precision and recall", {
  p <- make_pair()
  res <- code_agreement(p$llm, p$human, id_col = "id", themes = "A")
  expect_equal(res$n, 10)
  expect_equal(res$agreement, 0.8)
  expect_equal(res$kappa, (0.8 - 0.58) / 0.42)
  expect_equal(res$precision, 2 / 3)
  expect_equal(res$recall, 2 / 3)
  expect_equal(res$f1, 2 / 3)
  expect_lt(res$kappa_low, res$kappa)
  expect_gt(res$kappa_high, res$kappa)
})

test_that("perfect agreement gives kappa 1; constant codes give NA kappa", {
  p <- make_pair()
  expect_equal(code_agreement(p$llm, p$llm, "id", "A")$kappa, 1)

  same <- data.frame(id = 1:4, A = FALSE)
  res <- code_agreement(same, same, "id", "A")
  expect_equal(res$agreement, 1)
  expect_true(is.na(res$kappa))
  expect_true(is.na(res$precision))
})

test_that("units are matched by ID, not by row order", {
  p <- make_pair()
  shuffled <- p$human[10:1, ]
  expect_equal(code_agreement(p$llm, shuffled, "id", "A"),
               code_agreement(p$llm, p$human, "id", "A"))
})

test_that("NA codes and unmatched IDs are dropped", {
  p <- make_pair()
  p$llm$A[1] <- NA
  extra <- rbind(p$human, data.frame(id = 99, A = 1))
  expect_warning(res <- code_agreement(p$llm, extra, "id", "A"), "not found")
  expect_equal(res$n, 9)
})

test_that("defaults come from llm_auto_code attributes", {
  df <- mock_mixed_data(40, seed = 5)
  coded <- llm_auto_code(df, user_id, transcript_feedback,
                         c("UI_Confusion", "Positive_Experience"), coder = mock_coder())
  human <- data.frame(user_id = df$user_id,
                      UI_Confusion = df$profile == "frustrated",
                      Positive_Experience = df$profile == "efficient")
  res <- code_agreement(coded, human)
  expect_equal(res$theme, c("UI_Confusion", "Positive_Experience"))
  expect_true(all(res$kappa > 0.6))
})

test_that("bad inputs give clear errors", {
  p <- make_pair()
  expect_error(code_agreement(p$llm, p$human, "id"), "themes")
  expect_error(code_agreement(p$llm, p$human, themes = "A"), "id_col")
  expect_error(code_agreement(p$llm, rbind(p$human, p$human), "id", "A"), "one row per")
  expect_error(code_agreement(p$llm, data.frame(id = 1, A = "yes"), "id", "A"), "logical or 0/1")
  expect_error(code_agreement(p$llm, data.frame(id = 50, A = TRUE), "id", "A"), "No IDs")
})
