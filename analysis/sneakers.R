library(dplyr)
library(tidyr)
library(ggplot2)
library(GGally)
library(lubridate)
library(stargazer)
library(stringr)
library(car)


# Function to replace outliers with NAs
# Outliers are observations that lie outside of [Q1 - k * IQR, Q3 + k * IQR]
remove_outliers <- function(x, k = 1.5, na.rm = TRUE, ...) {
    qnt <- quantile(x, probs = c(0.25, 0.75), na.rm = na.rm, ...)
    h <- k * IQR(x, na.rm = na.rm)
    y <- x
    y[x > (qnt[2] + h)] <- NA
    y[x < (qnt[1] - h)] <- NA
    return(y)
}

get_color <- function(x) {
    re <- regex('(black|white|red|grey|blue|green|metallic|gold|orange|silver|pink|purple|brown|yellow)',
                ignore_case = TRUE)
    res <- str_to_lower(str_extract(x, re))
    res[is.na(res)] <- 'other'
    return(res)
}

z_test_sep_models <- function(m1, m2, coef1, coef2 = coef1, alternative = 'two.sided') {
    choices <- c('two.sided', 'greater', 'less')
    alt <- pmatch(alternative, choices)
    alternative <- choices[alt]
    if (length(alternative) > 1 || is.na(alternative)) {
        stop('Argument `alternative` must be one of: "greater", "less", "two.sided"')
    }
    
    method <- 'z-test for coefficients from two separate linear regression models'
    if (coef1 == coef2) {
        data.name <- paste0(coef1, ' from ', substitute(m1), ' and ', substitute(m2))
    } else {
        data.name <- paste0(coef1, ' and ', coef2, ' from ' , substitute(m1), ' and ', substitute(m2))
    }
    
    null.value <- 0
    attr(null.value, 'names') <- 'difference of coefficients'
    
    c1 <- unname(summary(m1)$coefficients[coef1, ])
    c2 <- unname(summary(m2)$coefficients[coef2, ])
    
    z <- (c1[1] - c2[1]) / sqrt(c1[2]^2 + c2[2]^2)
    attr(z, 'names') <- 'z'

    if (alternative == 'two.sided') {
        p <- 2 * pnorm(-abs(z))
    } else if (alternative == 'less') {
        p <- pnorm(-z)
    } else {
        p <- pnorm(z)
    }
    attr(p, 'names') <- NULL

    test <- list(method = method, data.name = data.name, null.value = null.value, alternative = alternative,
                 statistic = z, p.value = p)
    class(test) <- 'htest'

    return(test)
}


# delete variables with no variation
data <- read.csv('../data pages.csv') %>%
    # Remove columns with no variation and useless variables
        select(-X, -a_lim, -charity_condition, -condition, -data_type, -market_sales_last_period,
           -market_sales_this_period, -lithium_ion_battery, -meta_charity, -meta_deleted,
           -meta_hidden, -meta_lock_buying, -meta_lock_selling, -meta_mobile_only, -meta_raffle,
           -meta_redirected, -meta_restock, -minimum_bid, -product_category,
           -shipping_delivery_days_lower_bound, -shipping_delivery_days_upper_bound,
           -shipping_group, -shipping_has_additional_days_to_ship, -shipping_total_days_to_ship,
           -size_all_descriptor, -size_descriptor, -sku_variant_group, -type, -year,
           -market_last_highest_bid_time, -market_last_lowest_ask_time) %>%
    
    # Remove NAs and some strange values
    filter(!is.na(retail_price),
           !is.na(market_average_deadstock_price),
           market_average_deadstock_price > 0,
           # market_annual_high > 0,
           # market_annual_low > 0,
           release_date != '') %>%
    
    # In these NAs are actually zeros
    mutate(market_featured = case_when(is.na(market_featured) ~ 0, TRUE ~ market_featured)) %>%
    mutate(market_has_asks = case_when(is.na(market_has_asks) ~ 0, TRUE ~ market_has_asks)) %>%
    mutate(market_has_bids = case_when(is.na(market_has_bids) ~ 0, TRUE ~ market_has_bids)) %>%
    
    # It's not gender, more like target audience
    rename(target_demographic = gender) %>%
    mutate(market_days_since_creation = floor(as.numeric(ymd_hms(market_created_at) %--% ymd('2021-02-01'),
                                                         unit = 'days'))) %>%
    mutate(days_since_release = floor(as.numeric(ymd(release_date) %--% ymd('2021-02-01'), unit = 'days'))) %>%
    filter(size_locale != 'eu') %>%
    
    # Remove outliers
    mutate(market_total_dollars = remove_outliers(market_total_dollars, k = 3),
           market_lowest_ask_float = remove_outliers(market_lowest_ask_float, k = 3),
           market_highest_bid = remove_outliers(market_highest_bid, k = 3)) %>%
    na.omit() %>%
    
    filter(market_volatility > 0,
           market_total_dollars > 0) %>%
    
    mutate(color = get_color(colorway)) %>%
    mutate(bid_ask_spread = market_lowest_ask_float - market_highest_bid_float) %>%
    mutate(target_demographic = relevel(factor(target_demographic), ref = 'men')) %>%
    
    # select sizes
    group_by(shoe_size) %>%
    filter(n() >= 10) %>%
    ungroup() %>%
    
    # select brands
    group_by(brand) %>%
    filter(n() >= 10) %>%
    ungroup() %>%
    
    # select countries
    group_by(country_of_manufacture) %>%
    filter(n() >= 10) %>%
    ungroup()
    
    # select styles
    # group_by(style_id) %>%
    # filter(n() >= 10) %>%
    # ungroup()

table(data$primary_category, useNA = 'ifany')

qplot((data$market_number_of_asks - data$market_number_of_bids), bins = 100)

table(data$brand)
table(data$content_group)
table(data$target_demographic)
table(data$country_of_manufacture)
table(data$shoe_size)
table(data$color)

unique(data[data$market_featured == 1, c('title', 'ticker_symbol')])
unique(data[data$meta_browse_page_featured == 1, c('title', 'ticker_symbol')])

which(!(data$meta_browse_page_featured == data$market_featured))

length(unique(data$ticker_symbol))

for (var in names(data)) {
    if (is.numeric(data[[var]])) {
        print(var)
    }
}

num_col_names <- c('market_average_deadstock_price', 'market_lowest_ask_float', 'market_highest_bid_float',
                   'market_annual_high', 'market_annual_low', 'market_days_since_creation',
                   'market_deadstock_sold', 'market_number_of_asks', 'market_number_of_bids',
                   'market_sales_last_72_hours', 'market_total_dollars', 'market_volatility',
                   'days_since_release', 'retail_price')

boxplot(data[num_col_names], range = 3)

# TODO:
# * check out US HTS codes and descriptions
# * find a way to omit non-existent sizes


for (var in names(data)) {
    if (any(is.na(data[[var]]))) {
        print(var)
    }
}

ggplot(data, aes(x = market_sales_last_72_hours, y = market_lowest_ask_float)) +
    geom_point() +
    geom_smooth(method = 'lm', formula = y ~ x)

corr_mat <- Hmisc::rcorr(as.matrix(data[c('market_days_since_creation', 'market_deadstock_sold', # 'market_number_of_asks',
                                          # 'market_number_of_bids',
                                          'market_sales_last_72_hours', 'market_total_dollars',
                                          'market_volatility', 'days_since_release', 'retail_price')]))

corrplot::corrplot(corr_mat$r, method = 'number', p.mat = corr_mat$P)


data_log <- data
vars <- c('market_average_deadstock_price', 'market_lowest_ask_float', 'market_highest_bid_float',
          'market_deadstock_sold', 'market_sales_last_72_hours',
          'market_total_dollars', 'market_volatility', 'days_since_release', 'retail_price')

for (var in vars) {
    data[[paste(var, 'log', sep = '_')]] <- log(data[[var]])
}

vars_log <- c('market_average_deadstock_price_log', 'market_lowest_ask_float_log', 'market_highest_bid_float_log',
              'market_deadstock_sold_log', 'market_sales_last_72_hours_log',
              'market_total_dollars_log', 'market_volatility_log', 'days_since_release_log', 'retail_price_log')

ggpairs(data[vars])
ggpairs(data[vars_log])
ggpairs(data[c(vars[1:3], vars_log[-c(1 , 2, 3)])])  # looks good...
ggpairs(data[c(vars_log[1:3], vars[-c(1, 2, 3)])])

qplot(data = sample_n(data, size = 150), x = days_since_release, y = market_average_deadstock_price)
qplot(data = sample_n(data, size = 150), x = days_since_release, y = market_lowest_ask)
qplot(data = sample_n(data, size = 150), x = days_since_release, y = market_highest_bid)


# Regression
model1 <- lm(
    market_average_deadstock_price ~
        # market_deadstock_sold +
        # market_highest_bid_float +
        market_number_of_asks +
        market_number_of_bids +
        # market_total_dollars +
        market_volatility +
        days_since_release +
        retail_price +
        factor(color) +
        factor(brand) +
        factor(shoe_size) +
        factor(country_of_manufacture) +
        target_demographic +
        factor(media_has360) +
        factor(meta_browse_page_featured) +
        factor(market_featured),
    data = data
)

model2 <- lm(
    market_lowest_ask_float ~
        # market_deadstock_sold +
        # market_highest_bid_float +
        market_number_of_asks +
        market_number_of_bids +
        # market_total_dollars +
        market_volatility +
        days_since_release +
        retail_price +
        factor(color) +
        factor(brand) +
        factor(shoe_size) +
        factor(country_of_manufacture) +
        target_demographic +
        factor(media_has360) +
        factor(meta_browse_page_featured) +
        factor(market_featured),
    data = data
)

# stargazer::stargazer(model2, type = 'text')

model3 <- lm(
    market_highest_bid_float ~
        # market_deadstock_sold +
        # market_lowest_ask_float +
        market_number_of_asks +
        market_number_of_bids +
        # market_total_dollars +
        market_volatility +
        days_since_release +
        retail_price +
        factor(color) +
        factor(brand) +
        factor(shoe_size) +
        factor(country_of_manufacture) +
        target_demographic +
        factor(media_has360) +
        factor(meta_browse_page_featured) +
        factor(market_featured),
    data = data
)

model4 <- lm(
    bid_ask_spread ~
        # market_deadstock_sold +
        market_number_of_asks +
        market_number_of_bids +
        # market_total_dollars +
        market_volatility +
        days_since_release +
        retail_price +
        factor(color) +
        factor(brand) +
        factor(shoe_size) +
        factor(country_of_manufacture) +
        target_demographic +
        factor(media_has360) +
        factor(meta_browse_page_featured) +
        factor(market_featured),
    data = data
)

model5 <- lm(
    market_sales_last_72_hours ~
        market_number_of_asks +
        market_number_of_bids +
        # market_lowest_ask_float +
        # market_highest_bid_float +
        # market_total_dollars +
        factor(color) +
        market_volatility +
        days_since_release +
        retail_price +
        factor(brand) +
        factor(shoe_size) +
        factor(country_of_manufacture) +
        target_demographic +
        factor(media_has360) +
        factor(meta_browse_page_featured) +
        factor(market_featured),
    data = data
)

data$lowest_ask_rel_to_retail <- data$market_lowest_ask_float / data$retail_price - 1
data$highest_bid_rel_to_retail <- data$market_highest_bid_float / data$retail_price - 1

model6 <- lm(
    lowest_ask_rel_to_retail ~
        market_number_of_asks +
        market_number_of_bids +
        # market_highest_bid_float +
        # market_total_dollars +
        market_volatility +
        days_since_release +
        factor(color) +
        factor(brand) +
        factor(shoe_size) +
        factor(country_of_manufacture) +
        target_demographic +
        factor(media_has360) +
        factor(meta_browse_page_featured) +
        factor(market_featured),
    data = data
)

model7 <- lm(
    highest_bid_rel_to_retail ~
        market_number_of_asks +
        market_number_of_bids +
        # market_lowest_ask_float +
        # market_total_dollars +
        market_volatility +
        days_since_release +
        factor(color) +
        factor(brand) +
        factor(shoe_size) +
        factor(country_of_manufacture) +
        target_demographic +
        factor(media_has360) +
        factor(meta_browse_page_featured) +
        factor(market_featured),
    data = data
)

stargazer(model1, model2, model3, model4, type = 'html', out = './models (1).html')


ggplot(data, aes(x = log(bid_ask_spread), y = ..density..)) +
    geom_histogram() +
    geom_density(color = 'red')

model1_log <- lm(
    log(market_average_deadstock_price) ~
        # market_deadstock_sold +
        # market_highest_bid_float +
        market_number_of_asks +
        market_number_of_bids +
        # market_total_dollars +
        market_volatility +
        days_since_release +
        retail_price +
        factor(color) +
        factor(brand) +
        factor(shoe_size) +
        factor(country_of_manufacture) +
        target_demographic +
        factor(media_has360) +
        factor(meta_browse_page_featured) +
        factor(market_featured),
    data = filter(data,
                  market_average_deadstock_price > 0,
                  market_lowest_ask_float > 0,
                  market_highest_bid_float > 0,
                  bid_ask_spread > 0)
)

model2_log <- lm(
    log(market_lowest_ask_float) ~
        # market_deadstock_sold +
        # market_highest_bid_float +
        market_number_of_asks +
        market_number_of_bids +
        # market_total_dollars +
        market_volatility +
        days_since_release +
        retail_price +
        factor(color) +
        factor(brand) +
        factor(shoe_size) +
        factor(country_of_manufacture) +
        target_demographic +
        factor(media_has360) +
        factor(meta_browse_page_featured) +
        factor(market_featured),
    data = filter(data,
                  market_average_deadstock_price > 0,
                  market_lowest_ask_float > 0,
                  market_highest_bid_float > 0,
                  bid_ask_spread > 0)
)

model3_log <- lm(
    log(market_highest_bid_float) ~
        # market_deadstock_sold +
        # market_lowest_ask_float +
        market_number_of_asks +
        market_number_of_bids +
        # market_total_dollars +
        market_volatility +
        days_since_release +
        retail_price +
        factor(color) +
        factor(brand) +
        factor(shoe_size) +
        factor(country_of_manufacture) +
        target_demographic +
        factor(media_has360) +
        factor(meta_browse_page_featured) +
        factor(market_featured),
    data = filter(data,
                  market_average_deadstock_price > 0,
                  market_lowest_ask_float > 0,
                  market_highest_bid_float > 0,
                  bid_ask_spread > 0)
)

model4_log <- lm(
    log(bid_ask_spread) ~
        # market_deadstock_sold +
        market_number_of_asks +
        market_number_of_bids +
        # market_total_dollars +
        market_volatility +
        days_since_release +
        retail_price +
        factor(color) +
        factor(brand) +
        factor(shoe_size) +
        factor(country_of_manufacture) +
        target_demographic +
        factor(media_has360) +
        factor(meta_browse_page_featured) +
        factor(market_featured),
    data = filter(data,
                  market_average_deadstock_price > 0,
                  market_lowest_ask_float > 0,
                  market_highest_bid_float > 0,
                  bid_ask_spread > 0)
)

stargazer(model1_log, model2_log, model3_log, model4_log, type = 'html', out = './models_log.html')


model1_style <- lm(
    market_average_deadstock_price ~
        # market_deadstock_sold +
        # market_highest_bid_float +
        market_number_of_asks +
        market_number_of_bids +
        # market_total_dollars +
        market_volatility +
        days_since_release +
        retail_price +
        factor(color) +
        factor(brand) +
        factor(shoe_size) +
        factor(style_id) +
        factor(country_of_manufacture) +
        target_demographic +
        factor(media_has360) +
        factor(meta_browse_page_featured) +
        factor(market_featured),
    data = data
)

model2_style <- lm(
    market_lowest_ask_float ~
        # market_deadstock_sold +
        # market_highest_bid_float +
        market_number_of_asks +
        market_number_of_bids +
        # market_total_dollars +
        market_volatility +
        days_since_release +
        retail_price +
        factor(color) +
        factor(brand) +
        factor(shoe_size) +
        factor(style_id) +
        factor(country_of_manufacture) +
        target_demographic +
        factor(media_has360) +
        factor(meta_browse_page_featured) +
        factor(market_featured),
    data = data
)

# stargazer::stargazer(model2, type = 'text')

model3_style <- lm(
    market_highest_bid_float ~
        # market_deadstock_sold +
        # market_lowest_ask_float +
        market_number_of_asks +
        market_number_of_bids +
        # market_total_dollars +
        market_volatility +
        days_since_release +
        retail_price +
        factor(color) +
        factor(brand) +
        factor(shoe_size) +
        factor(style_id) +
        factor(country_of_manufacture) +
        target_demographic +
        factor(media_has360) +
        factor(meta_browse_page_featured) +
        factor(market_featured),
    data = data
)

model4_style <- lm(
    bid_ask_spread ~
        # market_deadstock_sold +
        market_number_of_asks +
        market_number_of_bids +
        # market_total_dollars +
        market_volatility +
        days_since_release +
        retail_price +
        factor(color) +
        factor(brand) +
        factor(shoe_size) +
        factor(style_id) +
        factor(country_of_manufacture) +
        target_demographic +
        factor(media_has360) +
        factor(meta_browse_page_featured) +
        factor(market_featured),
    data = data
)

stargazer(model1_style, model2_style, model3_style, model4_style, type = 'html', out = './models-style.html')

# aliaeses
transform_model <- function(m) {
    m <- rbind(names(m), m)
    m[1, 1] <- 'var'
    m <- as.data.frame(t(m))
    names(m) <- m[1, ]
    m <- m[2:nrow(m), ]
    m <- m[m[, 2] != '0', ]
    rownames(m) <- NULL
    return(m)
}

write.csv((alias(model2)$Complete), './model2-style.csv')
write.csv((alias(model3)$Complete), './model3-style.csv')
write.csv((alias(model4)$Complete), './model4-style.csv')

# multicollinearity analysis
cor(data$market_highest_bid_float, data$market_lowest_ask_float)

as.matrix(vif(model1)[, 3]^2)
as.matrix(vif(model2)[, 3]^2)
as.matrix(vif(model3)[, 3]^2)
as.matrix(vif(model4)[, 3]^2)
as.matrix(vif(model5)[, 3]^2)
as.matrix(vif(model6)[, 3]^2)
as.matrix(vif(model7)[, 3]^2)
# seems okay......


# colorways
library(tidytext)

colors <- read.csv('../data.csv') %>%
    select(color) %>%
    # mutate(colorway = color) %>%
    # mutate(color = get_color(colorway))
    unnest_tokens(color, color) %>%
    group_by(color) %>%
    count() %>%
    arrange(desc(n))


# Brands graph
rx <- regex('(adidas|asics|converse|crocs|jordan|new\\sbalance|nike|puma|timberland|vans|yeezy)',
            ignore_case = TRUE)
brand_coefs <- data.frame(
    brand  = str_sub(names(coef(model2)[str_detect(names(coef(model2)), rx)]), start = 14),
    ask    = coef(model2)[str_detect(names(coef(model2)), rx)],
    bid    = coef(model3)[str_detect(names(coef(model3)), rx)],
    spread = coef(model4)[str_detect(names(coef(model4)), rx)],
    row.names = NULL
)

brand_coefs %>%
    gather(key = model, value = estimate, 2:4) %>%
    
    ggplot(aes(x = brand, y = estimate, fill = model)) +
        geom_col(position = 'dodge') +
        labs(x = NULL) +
        scale_fill_discrete(name = 'Model',
                            breaks = c('ask', 'bid', 'spread'),
                            labels = c('Lowest Ask', 'Highest Bid', 'Bid-Ask Spread'))



# Hypothesis 1: volatility affects bid price more than ask price
z_test_sep_models(model2, model3, 'market_volatility')

# Hypothesis 2: brand affects ask prices more
z_test_sep_models(model2, model3, 'factor(brand)ASICS')
z_test_sep_models(model2, model3, 'factor(brand)Converse')  #, alternative = 'greater')
z_test_sep_models(model2, model3, 'factor(brand)Crocs')
z_test_sep_models(model2, model3, 'factor(brand)Jordan')
z_test_sep_models(model2, model3, 'factor(brand)New Balance')
z_test_sep_models(model2, model3, 'factor(brand)Nike')
z_test_sep_models(model2, model3, 'factor(brand)Timberland')
z_test_sep_models(model2, model3, 'factor(brand)Vans')
z_test_sep_models(model2, model3, 'factor(brand)Yeezy')


# Days since release as a polynomial
model1 <- lm(
    market_average_deadstock_price ~
        # market_deadstock_sold +
        # market_highest_bid_float +
        market_number_of_asks +
        market_number_of_bids +
        # market_total_dollars +
        market_volatility +
        days_since_release +
        retail_price +
        factor(color) +
        factor(brand) +
        factor(shoe_size) +
        factor(country_of_manufacture) +
        target_demographic +
        factor(media_has360) +
        factor(meta_browse_page_featured) +
        factor(market_featured),
    data = filter(data,
                  market_average_deadstock_price > 0,
                  market_lowest_ask_float > 0,
                  market_highest_bid_float > 0,
                  bid_ask_spread > 0)
)

model1_log <- lm(
    log(market_average_deadstock_price) ~
        # market_deadstock_sold +
        # market_highest_bid_float +
        market_number_of_asks +
        market_number_of_bids +
        # market_total_dollars +
        market_volatility +
        days_since_release +
        retail_price +
        factor(color) +
        factor(brand) +
        factor(shoe_size) +
        factor(country_of_manufacture) +
        target_demographic +
        factor(media_has360) +
        factor(meta_browse_page_featured) +
        factor(market_featured),
    data = filter(data,
                  market_average_deadstock_price > 0,
                  market_lowest_ask_float > 0,
                  market_highest_bid_float > 0,
                  bid_ask_spread > 0)
)

model1_dsr_poly <- lm(
    market_average_deadstock_price ~
        # market_deadstock_sold +
        # market_highest_bid_float +
        market_number_of_asks +
        market_number_of_bids +
        # market_total_dollars +
        market_volatility +
        poly(days_since_release, 2) +
        retail_price +
        factor(color) +
        factor(brand) +
        factor(shoe_size) +
        factor(country_of_manufacture) +
        target_demographic +
        factor(media_has360) +
        factor(meta_browse_page_featured) +
        factor(market_featured),
    data = filter(data,
                  market_average_deadstock_price > 0,
                  market_lowest_ask_float > 0,
                  market_highest_bid_float > 0,
                  bid_ask_spread > 0)
)

model1_log_dsr_poly <- lm(
    log(market_average_deadstock_price) ~
        # market_deadstock_sold +
        # market_highest_bid_float +
        market_number_of_asks +
        market_number_of_bids +
        # market_total_dollars +
        market_volatility +
        poly(days_since_release, 2) +
        retail_price +
        factor(color) +
        factor(brand) +
        factor(shoe_size) +
        factor(country_of_manufacture) +
        target_demographic +
        factor(media_has360) +
        factor(meta_browse_page_featured) +
        factor(market_featured),
    data = filter(data,
                  market_average_deadstock_price > 0,
                  market_lowest_ask_float > 0,
                  market_highest_bid_float > 0,
                  bid_ask_spread > 0)
)

stargazer(model1, model1_dsr_poly, model1_log, model1_log_dsr_poly,
          type = 'html', out = './models_dsr_sq.html')


data[data$uuid == '10e80190-6bd0-4d04-b4cb-2c674fd16bce', ]$'shoe_size'
data[data$ticker_symbol == 'AF1L-SUPBLW', ][c('shoe_size', 'uuid')]
