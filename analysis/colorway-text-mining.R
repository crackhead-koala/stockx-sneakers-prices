library(dplyr)
library(forcats)
library(tidytext)
library(stringr)
library(broom)

library(glmnet)
library(rsample)

library(ggplot2)


remove_outliers <- function(x, k = 1.5, na.rm = TRUE, ...) {
    qnt <- quantile(x, probs = c(0.25, 0.75), na.rm = na.rm, ...)
    h <- k * IQR(x, na.rm = na.rm)
    y <- x
    y[x > (qnt[2] + h)] <- NA
    y[x < (qnt[1] - h)] <- NA
    return(y)
}

create_colorway_tokens <- function(df) {
    # df - dataframe of uuid-colorway pairs
    
    result <- data.frame('uuid' = character(),
                         'color_token' = character())
    
    for (i in 1:nrow(df)) {
        colors <- str_split(df$colorway[i], '/', simplify = TRUE)
        for (col in colors) {
            result <- rbind(result, list(uuid = df$uuid[i], color_token = col))
        }
    }
    
    return(result)
}


data <- read.csv('../data pages.csv') %>%
    select(market_highest_bid, market_lowest_ask, colorway, uuid) %>%
    
    # use only prices higher than 0
    filter(market_highest_bid > 0, market_lowest_ask > 0) %>%
    
    # remove outliers
    mutate(market_highest_bid = remove_outliers(market_highest_bid, k = 3),
           market_lowest_ask = remove_outliers(market_lowest_ask, k = 3)) %>%
    na.omit() %>%
    
    mutate(bid_ask_spread = market_lowest_ask - market_highest_bid) %>%
    mutate(market_highest_bid_log = log(market_highest_bid),
           market_lowest_ask_log = log(market_lowest_ask),
           bid_ask_spread_log = log(bid_ask_spread)) %>%
    
    filter(!is.infinite(bid_ask_spread_log))

qplot(data$market_highest_bid_log)
qplot(data$market_lowest_ask_log)
qplot(data$bid_ask_spread_log)

tokens <- create_colorway_tokens(data[c('uuid', 'colorway')])

data_text <- data %>%
    right_join(tokens, by = 'uuid') %>%
    
    group_by(color_token) %>%
    filter(n() >= 10) %>%
    ungroup()


x_spread_log <- data_text %>%
    group_by(bid_ask_spread_log, color_token) %>%
    count() %>%
    cast_sparse(bid_ask_spread_log, color_token, n)

y_spread_log <- as.numeric(rownames(x_spread_log))


x_ask_log <- data_text %>%
    group_by(market_lowest_ask_log, color_token) %>%
    count() %>%
    cast_sparse(market_lowest_ask_log, color_token, n)

y_ask_log <- as.numeric(rownames(x_ask_log))


x_bid_log <- data_text %>%
    group_by(market_highest_bid_log, color_token) %>%
    count() %>%
    cast_sparse(market_highest_bid_log, color_token, n)

y_bid_log <- as.numeric(rownames(x_bid_log))

set.seed(51843)
m1 <- cv.glmnet(x_spread_log, y_spread_log, type.measure = 'mse', alpha = 1, family = 'gaussian')
m2 <- cv.glmnet(x_ask_log, y_ask_log, type.measure = 'mse', alpha = 1, family = 'gaussian')
m3 <- cv.glmnet(x_bid_log, y_bid_log, type.measure = 'mse', alpha = 1, family = 'gaussian')


coefs1 <- m1$glmnet.fit %>%
    tidy() %>%
    filter(lambda == m1$lambda.1se)

coefs1 %>%
    filter(term != "(Intercept)") %>% 
    group_by(estimate > 0) %>%
    top_n(10, abs(estimate)) %>%
    ungroup() %>%
    
    # plot
    ggplot(aes(x = fct_reorder(term, estimate), y = estimate, fill = estimate > 0)) +
        geom_col(show.legend = FALSE) +
        coord_flip() +
        labs(x = NULL) +
        ggtitle('log bid-ask spread')


coefs2 <- m2$glmnet.fit %>%
    tidy() %>%
    filter(lambda == m2$lambda.1se)

coefs2 %>%
    filter(term != "(Intercept)") %>% 
    group_by(estimate > 0) %>%
    top_n(10, abs(estimate)) %>%
    ungroup() %>%
    
    # plot
    ggplot(aes(x = fct_reorder(term, estimate), y = estimate, fill = estimate > 0)) +
        geom_col(show.legend = FALSE) +
        coord_flip() +
        labs(x = NULL) +
        ggtitle('log lowest ask')


coefs3 <- m3$glmnet.fit %>%
    tidy() %>%
    filter(lambda == m3$lambda.1se)

coefs3 %>%
    filter(term != "(Intercept)") %>% 
    group_by(estimate > 0) %>%
    top_n(10, abs(estimate)) %>%
    ungroup() %>%
    
    # plot
    ggplot(aes(x = fct_reorder(term, estimate), y = estimate, fill = estimate > 0)) +
        geom_col(show.legend = FALSE) +
        coord_flip() +
        labs(x = NULL) +
        ggtitle('log highest bid')


x_ask <- data_text %>%
    group_by(market_lowest_ask, color_token) %>%
    count() %>%
    cast_sparse(market_lowest_ask, color_token, n)

y_ask <- as.numeric(rownames(x_ask))


x_bid <- data_text %>%
    group_by(market_highest_bid, color_token) %>%
    count() %>%
    cast_sparse(market_highest_bid, color_token, n)

y_bid <- as.numeric(rownames(x_bid))


x_spread <- data_text %>%
    group_by(bid_ask_spread, color_token) %>%
    count() %>%
    cast_sparse(bid_ask_spread, color_token, n)

y_spread <- as.numeric(rownames(x_spread))


set.seed(51843)
m_ask <- cv.glmnet(x_ask, y_ask, type.measure = 'mse', gamma = 1, family = 'gaussian')
m_bid <- cv.glmnet(x_bid, y_bid, type.measure = 'mse', gamma = 1, family = 'gaussian')
m_spread <- cv.glmnet(x_spread, y_spread, type.measure = 'mse', gamma = 1, family = 'gaussian')


c_ask <- m_ask$glmnet.fit %>%
    tidy() %>%
    filter(lambda == m_ask$lambda.1se)

c_ask %>%
    filter(term != "(Intercept)") %>% 
    group_by(estimate > 0) %>%
    top_n(10, abs(estimate)) %>%
    ungroup() %>%
    
    # plot
    ggplot(aes(x = fct_reorder(term, estimate), y = estimate, fill = estimate > 0)) +
        geom_col(show.legend = FALSE) +
        coord_flip() +
        labs(x = NULL) +
        ggtitle('ask price')


c_bid <- m_bid$glmnet.fit %>%
    tidy() %>%
    filter(lambda == m_bid$lambda.1se)

c_bid %>%
    filter(term != "(Intercept)") %>% 
    group_by(estimate > 0) %>%
    top_n(10, abs(estimate)) %>%
    ungroup() %>%
    
    # plot
    ggplot(aes(x = fct_reorder(term, estimate), y = estimate, fill = estimate > 0)) +
        geom_col(show.legend = FALSE) +
        coord_flip() +
        labs(x = NULL) +
        ggtitle('bid price')


c_spread <- m_spread$glmnet.fit %>%
    tidy() %>%
    filter(lambda == m_spread$lambda.1se)

c_spread %>%
    filter(term != "(Intercept)") %>% 
    group_by(estimate > 0) %>%
    top_n(10, abs(estimate)) %>%
    ungroup() %>%
    
    # plot
    ggplot(aes(x = fct_reorder(term, estimate), y = estimate, fill = estimate > 0)) +
        geom_col(show.legend = FALSE) +
        coord_flip() +
        labs(x = NULL) +
        ggtitle('bid-ask spread')

