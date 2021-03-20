
from urllib.request import urlopen
from urllib.request import Request
from bs4 import BeautifulSoup as soup
import pandas as pd
import json
import requests
import argparse
import os.path
import glob
import numpy as np


def getItemData(item:dict) -> list:
    """
    Parameters:
    -----------------------
    item:dictionary containing item data
    function: extracts the neccessary json data
    Returns:
    -------------------------
    a list containing all the relevant item data
    """
    try:
        brand = item['item']['brand']
    except:
        brand = np.nan
    
    try:
        color = item['item']['color']
    except:
        color = np.nan

    try:
        description = item['item']['description']
    except:
        description = np.nan

    try:
        image_url = item['item']['image']
    except:
        image_url = np.nan
        
    try:
        item_condition = item['item']['itemCondition']
    except:
        item_condition = np.nan
    
    try:
        model = item['item']['model']
    except:
        model = np.nan
    
    try:
        name = item['item']['name']
    except:
        name = np.nan
    
    try:
        releaseDate = item['item']['releaseDate']
    except:
        releaseDate = np.nan

    try:
        item_sku = item['item']['sku']
    except:
        item_sku = np.nan

    try:
        item_url = item['item']['url']
    except:
        item_url = np.nan
    
    try:
        lowPrice = item['item']['offers']['lowPrice']
    except:
        lowPrice = np.nan
    
    try:
        highPrice = item['item']['offers']['highPrice']
    except:
        highPrice = np.nan
    
    try:
        currency = item['item']['offers']['priceCurrency']
    except:
        currency = np.nan
    
    return [item_sku, name, model, color, brand, item_url, description, image_url, item_condition, releaseDate, lowPrice, highPrice, currency]


def getColumnData(itemsData, index):
    """
    Paramaters:
    --------------
    itemsData: list of list containing items data
    index: index of column you want to make
    Returns:
    list which represents the column of a particular item
    """
    
    return [ itemsData[i][index] for i in range(len(itemsData)) ]


def createDataFrame(itemsData:list):
    """
    Parameters:
    --------------------
    itemsData: list of lists containing the items data
    Function: Combine the lists into one pandas dataframe object
    Returns:
    -------------------
    panda dataframe containing all item data
    """
    keys = ['item_sku', 'name', 'model', 'color', 'brand', 'item_url', 'description', 'image_url', 'item_condition', 'releaseDate', 'lowPrice', 'highPrice', 'currency']
    values = [ getColumnData(itemsData, i) for i in range(len(itemsData[0])) ]
   
    return pd.DataFrame({ keys[i]:values[i] for i in range(len(keys)) })


def scrapeHTML(html):
    """"
    Parameters:
    ----------------
    html: parsed html page txt
    Function: scrapes the page and returns a pandas dataframe containing the item data
    Returns:
    ----------------
    pandas dataframe of the given html page
    """

    json_container = html.find_all("script", type="application/ld+json")
    # print(html)
    data = ''

    for conatiner in json_container:
        data = json.loads(conatiner.string.extract())
        if data['@type'] == 'OfferCatalog':
            break
    
    items = data['itemListElement']
    itemsData = [ getItemData(item) for item in items ]
    df = createDataFrame(itemsData)
    
    return df


def scrapePages(startPage, endPage):
    """
    Parameters:
    ---------------
    startPage: specifies what page to start scraping from
    endPage: specifies what page to end scraping at
    Retunrs:
    -------------
    pandas dataframe containing all item data of products from startPage to endPage
    """
    headers = {
        'User-Agent': 'Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.3'
    }

    base_url = 'https://stockx.com/sneakers?page='

    frames = []  # list of DFs containing data on each page
    for i in range(startPage, endPage + 1):
        currentPage = base_url + str(i)
        
        # html = requests.get(currentPage).content
        req = Request(url = currentPage, headers=headers)
        
        html = urlopen(req).read()
        html = soup(html, 'lxml')

        try:
            currentPageDF = scrapeHTML(html)
            frames.append(currentPageDF)
        except:
            print('Unable to scrape page ', i)
    
    return pd.concat(frames, axis=0, ignore_index=True)


def scrapeDownloadedPages(path):
    """
    Parameter:
    -------------
    path: path to directory contianing html pages
    """
    files = [ f for f in glob.glob(path + '/*.html') ]
    frames = []
    for file in files:
        with open(file) as f:
            try:
                html = soup(f,'lxml')
                frames.append(scrapeHTML(html))
            except:
                print('Unable to scrape page ', file)
    result = pd.concat(frames)
    return result


def main():
    parser = argparse.ArgumentParser(description='Shoe Scrapper for https://stockx.com/')

    parser.add_argument('--start', type=int, help='Starting index of page you want to scrape from.', required=True)
    parser.add_argument('--end', type=int, help='End index of page you want to scrape to.', required=True)
    parser.add_argument('--o', type=str, help='Location of the output file you want to store your scraped results (MUST BE CSV)', required=True)
    
    args = parser.parse_args()

    df = scrapePages(args.start, args.end)

    if os.path.isfile(args.o):
        df.to_csv(args.o, mode='a', header=False)
    else:
        df.to_csv(args.o)

    # result = scrapeDownloadedPages('./html')
    # result.to_csv('out.csv')
    
    
if __name__ == '__main__':
    main()
