# -*- coding: utf-8 -*-
"""
Created on Thu May 05 22:25:29 2011

@author: Ryan Wang
"""

import cookielib, csv, itertools, mechanize, re, sqlite3, sys
import datetime, random, time
from BeautifulSoup import BeautifulSoup

BASE_URL = 'http://www.amazon.com'

# Wrapper function that calls all the little functions
# In: br - browser object, isbn - the ISBN key, isbn_dict - a dictionary of past data for the ISBN, indexed by date
# Out: isbn_dict - an updated version of isbn_dict with latest update
def getAmazon(br, dbase, isbn):
    print isbn
    sleepy = 0 # How long the function sleeps for, this might be worth changing
    
    # Get the data from the main page... e.g. list price, current price
    try:
        main_url = BASE_URL + 'gp/product/' + str(isbn)
        main = BeautifulSoup(br.open(main_url))
        #print 'page opened'
    except:
        print "Couldn't load page for " + str(isbn), sys.exc_info()[0]
        #return dict()
        return 0
    else:
        profileData = parseProfile(main, isbn)
        storeData(profileData, dbase, table = 'profiles')
        
    # Get the listings, this is a much more complicated process than for the profiles
    # Try to get the links from the main soup, need to do this before br goes to another page
    try:
        new_link = br.find_link(url_regex = '/gp/offer-listing/' + str(isbn) + '/.+condition=new')
    except:
        new_link = 'none'
    try:
        used_link = br.find_link(url_regex = '/gp/offer-listing/' + str(isbn) + '/.+condition=used')
    except:
        used_link = 'none'

    # Now move on to the listings and parse    
    try:
        new_soup = BeautifulSoup(br.follow_link(new_link))
        getListing(br, new_soup, dbase, isbn, used = 0, sleeptime = sleepy)
    except:
        print "Couldn't load new listings for " + str(isbn)

    try:
        used_soup = BeautifulSoup(br.follow_link(used_link))
        getListing(br, used_soup, dbase, isbn, used = 1, sleeptime = sleepy)
    except:
        print "Couldn't load used listings for " + str(isbn)

    print 'Amazon data retrieved for ISBN: ' + str(isbn)

# Function for getting the basic info from main-page e.g. list price, current price, rating
def parseProfile(soup, isbn):
    # IN THIS ORDER, COMES FROM DATABASE createdb.py

    now = datetime.datetime.now()
    dateacc = now.strftime("%Y-%m-%d")
    
    # Get current prices for the book        
    priceblock = soup.find('div', attrs = {'class':'buying','id':'priceBlock'})
    # Back when I tried to get listprices.
    #try:
    #    listprice = float(priceblock.find(attrs={'class':'listprice'}).text.strip('$'))
    #except:
    #    listprice = 'NA'
    try:
        currprice = float(priceblock.find(attrs={'class':'priceLarge'}).text.strip('$'))
    except:
        currprice = 'NA'
    
    # Consider the first div class after the <h2>Product Details</h2> tag
    detailblock = str(soup.find(text = 'Product Details').next.next)
    rating = re.search('([\d.]+) out of 5 stars', detailblock)
    if rating: 
        rating = float(rating.group(1))
    else:
        rating = 'NA'
    reviews = re.search('(\d+) customer reviews', detailblock)
    if reviews: 
        reviews = int(reviews.group(1))
    else:
        rating = 'NA'
    rank = re.search('#([\d\,]+) in Books', detailblock)
    if rank: 
        rank = int(rank.group(1).replace(",",""))
    else:
        rank = 'NA'

    wanted = [dateacc, isbn, reviews, rating, rank, currprice]
    return wanted

# Give some data to store and the sqlite3 dbase's cursor object to store it
# IN: data - a list in the correct order, dbase - a sqlite3 cursor object
def storeData(data, dbase, table):
    # Get row names from profiles table
    val = ",".join([q for q in list(itertools.repeat('?',len(data)))])
    dbase.execute('INSERT INTO ' + str(table) + ' VALUES(' + str(val) + ')', data)

# Given soup contents,
def updateListing(soup, dbase, isbn, used, price_rank):
    results = soup.findAll('tbody', attrs={'class':'result'})

    # for each entry, store a listing in the sqlite3 dbase
    for entry in results:

        ### Rewrite this part - break these two functions up
        
        now = datetime.datetime.now()
        dateacc = now.strftime("%Y-%m-%d")

        price = float(entry.find('span', attrs={'class':'price'}).text.strip('$'))    
        condition = entry.find('div', attrs={'class':'condition'}).text
        
        # Check if this is the amazon listing... then there's no rating, etc.
        checkamzn = entry.find(alt = 'Amazon.com')
        if checkamzn: 
            seller = 'Amazon.com'
            rating = 'NA'
            fba = 1
            comments = 'NA'
            numrating = 'NA'
            shipping = 0.0
            shipsfrom = 'NA'
        else:
            seller = entry.find('div', attrs={'class':'seller'}).text.split(':')[1]
            
            shipping_html = entry.find('span', attrs={'class':'price_shipping'})
            if shipping_html:
                shipping = float(shipping_html.text.strip('+').strip().strip('$'))
            else:
                shipping = 'NA'

            fba_html = entry.find('div', attrs={'class':'fba_link'}) #Fulfilled by Amazon
            if fba_html:
                fba = 1
                shipping = 0.0
            else:
                fba = 0
            
            rating_html = entry.find('div', attrs={'class':'rating'})
            if rating_html:
                if re.search('Just Launched', rating_html.text):
                    rating = 'NA'
                    numrating = 0
                elif re.search('1 rating', rating_html.text):
                    rating = re.search('(\d+%)', rating_html.text).group(1)
                    numrating = 1
                else:
                    rating = re.search('(\d+%)', rating_html.text).group(1)
                    numrating = re.search('([\d\,]+) total ratings', rating_html.text).group(1)
            
            availability = entry.find('div', attrs={'class':'availability'})
            shipsfrom_html = re.search('Ships from ([\w\,\s]+).', availability.text)
            if shipsfrom_html:
                shipsfrom = shipsfrom_html.group(1)
            else:
                shipsfrom = 'NA'
            
            comments_html = entry.find('div', attrs={'class':'comments'})
            if comments_html:
                comments = comments_html.text
            else:
                comments = 'NA'

        #### Fix price rank later.... ####
        listing = [dateacc, isbn, seller, rating, fba, numrating, shipping, price, condition, 'NA']
        # Increment the price_rank
        price_rank = price_rank + 1
        listing_desc = [isbn, seller, used, comments, shipsfrom]
        # Update listings table
        storeData(listing, dbase, 'listings') # DONT CHANGE THE TABLE NAMES !!!!!
        # Update listings description table
        storeData(listing_desc, dbase, 'listing_desc')
        
    return price_rank

# In: br - the current browser object, isbn - isbn for the book in question, condition - new/old
# Out: listing_data - a dictionary of listing details, indexed by seller
def getListing(br, soup, dbase, isbn, used, sleeptime):
    # Update listing_data by parsing the listing soup (store 15 listings at a time)
    p_rank = 1
    p_rank = updateListing(soup, dbase, isbn, used, p_rank)

    # Now move on to the next 15 entries, if there are any
    next_link = [l for l in br.links(text_regex = '^Next')]
    while len(next_link) > 0:
        next_page = br.follow_link(next_link[0])
        time.sleep(random.uniform(0,sleeptime))
        next_listing = BeautifulSoup(next_page)
        p_rank = updateListing(next_listing, dbase, isbn, used, p_rank) # Update with the new results
        next_link = [l for l in br.links(text_regex = '^Next')]

    print str(p_rank) + " listings"
        
    return 1


if __name__=='__main__':
    """
    Usage: python scrape_amazon.py [isbnpath] [dbpath]
    """
    if len(sys.argv) > 1:
        isbnpath = sys.argv[1]
        dbpath = sys.argv[2]
    else:
        isbnpath = 'isbns.csv'
        dbpath = 'main'
        
    ## Set up the browser
    br = mechanize.Browser()
    
    ## Cookie Jar
    cj = cookielib.LWPCookieJar()
    br.set_cookiejar(cj)
    
    ## Browser options
    br.set_handle_equiv(True)
    br.set_handle_gzip(True)
    br.set_handle_redirect(True)
    br.set_handle_referer(True)
    br.set_handle_robots(False)
    br.addheaders = [('User-agent', 'Mozilla/5.0 (Windows; U; Windows NT 6.0; en-US; rv:1.9.2.17) Gecko/20110420 Firefox/3.6.17 (.NET CLR 3.5.30729)')]    
    
    while True:
        ### Import the list of ISBNS        
        isbnfile = open(isbnpath, 'r')
        reader = csv.reader(isbnfile)
        isbnlist = list()
        for row in reader:
            isbnlist.append(row)
        isbnfile.close()
        random.shuffle(isbnlist)
    
        ### Run the updater for each ISBN
        conn = sqlite3.connect('main')
        c = conn.cursor()
    
        count = 0
        for entry in isbnlist:
            count = count + 1
            print "Entry:" + str(count)
            ## Get the data from the main page... e.g. list price, current price
            isbn = entry[0]
            getAmazon(br, c, isbn)
            print "@" + datetime.datetime.now().strftime("%H:%M:%S")
            # Commit every 100 listings to avoid MemoryError
            if count % 100 == 0:
                conn.commit()
                br.clear_history()
                print 'Data committed to main, browser history cleared'
    
        conn.commit()
        conn.close()
     