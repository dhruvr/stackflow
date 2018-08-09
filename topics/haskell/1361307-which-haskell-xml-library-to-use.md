
# Which Haskell XML library to use?

## Question
        
I see that there is a few of XML processing libraries in Haskell.

*   [HaXml](http://www.haskell.org/HaXml/) seems to be the most popular (according to [dons](http://donsbot.wordpress.com/2009/08/29/haskell-popularity-rankings-september-2009/))
*   [HXT](http://www.fh-wedel.de/~si/HXmlToolbox/) seems to be the most advanced (but also the most difficult to learn thanks to arrows)
*   [xml](http://hackage.haskell.org/package/xml) which seems to be just the basic parser
*   [HXML](http://www.flightlab.com/~joe/hxml/) seems to be abandoned
*   tagsoup and tagchup
*   libXML and libXML SAX bindings

So, which library to choose if I want it

*   to be reasonably powerful (to extract data from XML and to modify XML)
*   likely to be supported long time in the future
*   to be a “community choice” (default choice)

And while most of the above seem to be sufficient for my current needs, what are the reason to choose one of them over the others?

**UPD 20091222**:

Some notes about licenses:

*   BSD or MIT: [hexpat](http://hackage.haskell.org/package/hexpat), [hxt](http://hackage.haskell.org/package/hxt), [libxml](http://hackage.haskell.org/package/libxml), [tagsoup](http://hackage.haskell.org/package/tagsoup), [xml](http://hackage.haskell.org/package/xml)
*   LGPL: [HaXml](http://hackage.haskell.org/package/HaXml)
*   GPLv2:
*   GPLv3: [libxml-sax](http://hackage.haskell.org/package/libxml-sax), [tagchup](http://hackage.haskell.org/package/tagchup), [tagsoup-ht](http://hackage.haskell.org/package/tagsoup-ht)

## Answer
        
I would recommend:

1.  [xml](http://hackage.haskell.org/package/xml), if your task is simple
2.  [haxml](http://hackage.haskell.org/package/HaXml), if your task is complex
3.  [hxt](http://hackage.haskell.org/package/hxt), if you like arrows
4.  [hexpat](http://hackage.haskell.org/package/hexpat) if you need high performance
