library(dplyr)
library(forcats)
library(stringr)
library(tidytext)
library(stopwords)
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


data <- read.csv('../data pages.csv') %>%
    select(market_highest_bid, market_lowest_ask, description, colorway) %>%
    filter(description != '') %>%
    
    # use only prices higher than 0
    filter(market_highest_bid > 0, market_lowest_ask > 0) %>%
    
    # remove outliers
    mutate(market_highest_bid = remove_outliers(market_highest_bid, k = 3),
           market_lowest_ask = remove_outliers(market_lowest_ask, k = 3)) %>%
    na.omit() %>%
    
    mutate(bid_ask_spread = market_lowest_ask - market_highest_bid) %>%
    filter(bid_ask_spread > 0) %>%
    
    mutate(market_highest_bid_log = log(market_highest_bid),
           market_lowest_ask_log = log(market_lowest_ask),
           bid_ask_spread_log = log(bid_ask_spread))

qplot(data$market_highest_bid)
qplot(data$market_highest_bid_log)
qplot(data$market_lowest_ask)
qplot(data$market_lowest_ask_log)
qplot(data$bid_ask_spread)
qplot(data$bid_ask_spread_rel)


# Prepare text for mining
stop_words <- as_tibble(stopwords('en'))
add <- tibble(value = c("you're", 'you`re', 'you’re', 'please', 'note', 'sneakers',
                        '2018', '2020', '140', '300', '90', '350', 'three', 'march',
                        '190', 'max', '07', '1973', '1980s', '1985', '26th'))
stop_words <- rbind(stop_words, add)

data_text <- data %>%
    mutate(description = str_to_lower(description)) %>%
    unnest_tokens(word, description) %>%
    
    # use only words that occur 10 or more times
    group_by(word) %>%
    filter(n() >= 10) %>%
    ungroup() %>%
    
    # remove stopwords
    anti_join(stop_words, by = c('word' = 'value'))

rm(stop_words, add)


x_lowest_ask <- data_text %>%
    group_by(market_lowest_ask, word) %>%
    count() %>%
    cast_sparse(market_lowest_ask, word, n)

y_lowest_ask <- as.numeric(rownames(x_lowest_ask))


x_highest_bid <- data_text %>%
    group_by(market_highest_bid, word) %>%
    count() %>%
    cast_sparse(market_highest_bid, word, n)

y_highest_bid <- as.numeric(rownames(x_highest_bid))


x_bid_ask_spread <- data_text %>%
    group_by(bid_ask_spread, word) %>%
    count() %>%
    cast_sparse(bid_ask_spread, word, n)

y_bid_ask_spread <- as.numeric(rownames(x_bid_ask_spread))


# x_lowest_ask_log <- data_text %>%
#     group_by(market_lowest_ask_log, word) %>%
#     count() %>%
#     cast_sparse(market_lowest_ask_log, word, n)
# 
# y_lowest_ask_log <- as.numeric(rownames(x_lowest_ask_log))
# 
# 
# x_highest_bid_log <- data_text %>%
#     group_by(market_highest_bid_log, word) %>%
#     count() %>%
#     cast_sparse(market_highest_bid_log, word, n)
# 
# y_highest_bid_log <- as.numeric(rownames(x_highest_bid_log))
# 
# 
# x_bid_ask_spread_log <- data_text %>%
#     group_by(bid_ask_spread_log, word) %>%
#     count() %>%
#     cast_sparse(bid_ask_spread_log, word, n)
# 
# y_bid_ask_spread_log <- as.numeric(rownames(x_bid_ask_spread_log))


# lasso with cross validation (based on MSE)
set.seed(42069)

m_ask <- cv.glmnet(x_lowest_ask, y_lowest_ask, type.measure = 'mse', gamma = 1, family = 'gaussian')
m_bid <- cv.glmnet(x_highest_bid, y_highest_bid, type.measure = 'mse', gamma = 1, family = 'gaussian')
m_spread <- cv.glmnet(x_bid_ask_spread, y_bid_ask_spread, type.measure = 'mse', gamma = 1, family = 'gaussian')

# m_ask_log <- cv.glmnet(x_lowest_ask_log, y_lowest_ask_log, type.measure = 'mse', gamma = 1, family = 'gaussian')
# m_bid_log <- cv.glmnet(x_highest_bid_log, y_highest_bid_log, type.measure = 'mse', gamma = 1, family = 'gaussian')
# m_spread_log <- cv.glmnet(x_bid_ask_spread_log, y_bid_ask_spread_log, type.measure = 'mse', gamma = 1, family = 'gaussian')


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


# c_ask_log <- m_ask_log$glmnet.fit %>%
#     tidy() %>%
#     filter(lambda == m_ask_log$lambda.1se)
# 
# c_ask_log %>%
#     filter(term != "(Intercept)") %>% 
#     group_by(estimate > 0) %>%
#     top_n(10, abs(estimate)) %>%
#     ungroup() %>%
#     
#     # plot
#     ggplot(aes(x = fct_reorder(term, estimate), y = estimate, fill = estimate > 0)) +
#     geom_col(show.legend = FALSE) +
#     coord_flip() +
#     labs(x = NULL) +
#     ggtitle('log ask price')
# 
# 
# c_bid_log <- m_bid_log$glmnet.fit %>%
#     tidy() %>%
#     filter(lambda == m_bid_log$lambda.1se)
# 
# c_bid_log %>%
#     filter(term != "(Intercept)") %>% 
#     group_by(estimate > 0) %>%
#     top_n(10, abs(estimate)) %>%
#     ungroup() %>%
#     
#     # plot
#     ggplot(aes(x = fct_reorder(term, estimate), y = estimate, fill = estimate > 0)) +
#     geom_col(show.legend = FALSE) +
#     coord_flip() +
#     labs(x = NULL) +
#     ggtitle('log bid price')
# 
# c_spread_log <- m_spread_log$glmnet.fit %>%
#     tidy() %>%
#     filter(lambda == m_spread_log$lambda.1se)
# 
# c_spread_log %>%
#     filter(term != "(Intercept)") %>% 
#     group_by(estimate > 0) %>%
#     top_n(10, abs(estimate)) %>%
#     ungroup() %>%
#     
#     # plot
#     ggplot(aes(x = fct_reorder(term, estimate), y = estimate, fill = estimate > 0)) +
#     geom_col(show.legend = FALSE) +
#     coord_flip() +
#     labs(x = NULL) +
#     ggtitle('log bid-ask spread')


data$description[str_detect(data$description, '300')]  # retail price of $300
data$description[str_detect(data$description, regex('scott', ignore_case = TRUE))]  # Travis Scott
data$description[str_detect(data$description, '1985')]  # "Be True To Your School", 1985
data$description[str_detect(data$description, regex('retailers', ignore_case = TRUE))]  # "available at select retailers"
data$description[str_detect(data$description, regex('collaboration', ignore_case = TRUE))]  # colabs with other companies (e.g. Nike and Supreme or ASICS), rock bands and singers (e.g. Nike SB and Grateful Dead, Crocs and Bad Bunny), boutiques (Nike amd Slam Jam), football clubs (Jordan and Paris Saint Germain) and others
data$description[str_detect(data$description, regex('\\scame\\s', ignore_case = TRUE))]  # "came together", "came out with (a release)", "came around", "came in (colorways, sizes, date)", "came back"
data$description[str_detect(data$description, regex('\\slike\\s', ignore_case = TRUE))]  # "similar to (other model)", "looks like", "for example", used for simile and other figures of speech, "be fond of", ""
data$description[str_detect(data$description, regex('\\scop\\s', ignore_case = TRUE))]  # "cop these on StockX today"
data$description[str_detect(data$description, regex('190', ignore_case = TRUE))]  # retail price of $190
data$description[str_detect(data$description, regex('\\smarch\\s', ignore_case = TRUE))]  # release month
data$description[str_detect(data$description, regex('\\smax\\s', ignore_case = TRUE))]  # Nike Air Max
data$description[str_detect(data$description, regex('\\sprinted\\s', ignore_case = TRUE))]  # "At last, a graphic drawing of the Earth is printed on each tongue below branded tabs that sport the design’s name", "nThe Nike Dunk Low Community Garden features a dyed and printed patchworked canvas upper with hits of red, teal, and gold on the toe box, Swoosh, and heel tab", "the Limeade features “air” printed in bubble graphics that carry around the side panels and heel of the shoe", "featuring a Nike Air logo printed in Japanese", "The sneaker’s white midsole has the colorway’s style code and release date printed in black text." --- additional printed elements
data$description[str_detect(data$description, regex('\\sstyle\\s', ignore_case = TRUE))]  # style code printed, "walk in style", "style of", "street style", "Practice those free throws in style", "Original “Wings” can be found towards the ankle of the sneaker, giving the wearer the ability to style the sneaker as they see fit"
data$description[str_detect(data$description, regex('\\sheat\\s', ignore_case = TRUE))]  # "Rock some light heat after buying the Air Fear of God Raid Light Bone", "Add this heat to your sneaker collection", "Add some major heat for your summer closet", Miami Heat, "Daenerys Targaryen is here and dropping more heat than her dragons with the adidas Ultra Boost 4.0 Game of Thrones Targaryen", "Jordan Brand turns up the heat on a classic silhouette", "adidas had brewed up more sneaker collection heat with the adidas Yeezy Boost 350 Black Red"
data$description[str_detect(data$description, regex('\\stranslucent\\s', ignore_case = TRUE))]  # "translucent sole"
data$description[str_detect(data$description, regex('\\smarks\\s', ignore_case = TRUE))]

data$description[str_detect(data$description, regex('\\svirgil\\s', ignore_case = TRUE))]  # Virgil Abloh
data$description[str_detect(data$description, regex('\\sselect\\s', ignore_case = TRUE))]  # select retailers
data$description[str_detect(data$description, regex('\\seven\\s', ignore_case = TRUE))]  # "even more" --- emphasis
data$description[str_detect(data$description, regex('\\sneon\\s', ignore_case = TRUE))]  # shoe/lace color, "Match the neon lights in the city of Seoul after copping the Air Max 97 Neon Seoul", Nike Air Max Neon --- model
data$description[str_detect(data$description, regex('\\sse\\s', ignore_case = TRUE))]  # Jordan SE
data$description[str_detect(data$description, regex('\\sthree\\s', ignore_case = TRUE))]  # "three stripes", "one of three (colorways, designs, models)", "previous three (models)", ""
data$description[str_detect(data$description, regex('\\sbrought\\s', ignore_case = TRUE))]  # "brought back", "brought to new heights", "Sean Wotherspoon brought his talents"

data$description[str_detect(data$description, regex('\\supper\\s', ignore_case = TRUE))]  # a lot of things...


dscr <- distinct(data, description)$description

dscr[str_detect(dscr, regex('\\scone\\s', ignore_case = TRUE))]  # a lot of things...
dscr[str_detect(dscr, regex('\\stongue’s\\s', ignore_case = TRUE))]  # a lot of things...


data %>%
    filter(str_detect(description, regex('cone', ignore_case = TRUE))) %>%
    group_by(description) %>%
    count()
