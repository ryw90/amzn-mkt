# -*- coding: utf-8 -*-
"""
Created on Sat May 14 10:55:19 2011

@author: Ryan Wang
"""

### Database setup

import sqlite3

if __name__ == '__main__':
    conn = sqlite3.connect('main')
    c = conn.cursor()
    
    # Create the books table
    # Is static - holds the basic information for each ISBN
    c.execute('CREATE TABLE books (isbn TEXT NOT NULL, listprice FLOAT)')
    
    # Create the profiles table
    # Is updated daily (?) - holds the price/rank etc. for a given visit
    c.execute('CREATE TABLE profiles (dateacc TEXT NOT NULL, isbn TEXT NOT NULL, reviews INTEGER, rating FLOAT, rank INTEGER, currprice FLOAT, UNIQUE(dateacc, isbn) ON CONFLICT IGNORE)')
                                      
    # Create the listings table
    # Is updated daily (?) - holds each listing
    # Should index this by seller? dateacc?
    c.execute('CREATE TABLE listings (dateacc TEXT NOT NULL, isbn TEXT NOT NULL REFERENCES books(isbn), seller TEXT NOT NULL, rating FLOAT, fba INTEGER, numrating INTEGER, shipping FLOAT, price FLOAT, condition TEXT, price_rank INTEGER, UNIQUE(dateacc, isbn, seller, price) ON CONFLICT IGNORE)')
    
    # Create the listings_desc table
    # For each listing (isbn, seller, used), there should be 1 comment (we assume it doesn't change)
    c.execute('CREATE TABLE listing_desc (isbn TEXT NOT NULL REFERENCES books(isbn), seller TEXT NOT NULL, used INTEGER NOT NULL, comments TEXT, shipsfrom TEXT, PRIMARY KEY(isbn, seller, used) ON CONFLICT IGNORE)')
                                      
