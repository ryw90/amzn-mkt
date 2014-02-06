import csv, urllib
from xml.dom.minidom import parse

BOOKPATH = 'nyt_bestsellers.csv'
DATEPATH = 'list_dates.csv'

# In: date - YYYY-MM-DD
# Return an XML DOM object
def getList(api_key, list_name, date):
    url = 'http://api.nytimes.com/svc/books/v2/lists/%s/%s.xml?api-key=%s' % (date,list_name,api_key)
    list_dom = parse(urllib.urlopen(url))
    return list_dom

# Extract the titles from the list and write to .csv
def getTitles(date, outfile, list_dom):
    books = list_dom.getElementsByTagName("book")
    for book in books:
      details = extractBookDetails(book)
      details.append(date)
      print details
      details2 = ['"' + elem + '"' for elem in details]
      outfile.write(','.join(details2) + "\n")

# Extract book details from a book XML object
def extractBookDetails(book):
   details = list()
   for node in book.childNodes:
      if node.nodeName == "isbns":
         pass # This is covered by primary_isbns in book_details
      elif node.nodeName == "reviews":
         pass # Don't need these
      elif node.nodeName == "book_details":
         for node2 in node.firstChild.childNodes:
            try:
               if node2.nodeName == "description": continue
               details.append(node2.firstChild.data.encode('utf-8'))
            except:
               details.append('NA')
      else:
         details.append(node.firstChild.data.encode('utf-8'))
   return details

# Output path
def updateFile(list_name):
   outfile = open(BOOKPATH, 'a')

   # Read in dates
   datefile = open(DATEPATH, 'r')

   reader = csv.reader(datefile)
   for row in reader:
      date = row[0]
      print date
      bob = getList(api_key,list_name,date)
      getTitles(date, outfile, bob)

   outfile.close()
   datefile.close()
   print "File updated with: %s" % list_name
   
if __name__=='__main__':   
    list_names = ('hardcover-fiction','hardcover-nonfiction','trade-fiction-paperback','mass-market-paperback','paperback-nonfiction','hardcover-graphic-books','hardcover-business-books','hardcover-political-books')
    api_key = "6ee708f52017516e1bd4b78222862306:14:62790365"
    map(lambda list_name: updateFile(list_name), list_names)
    