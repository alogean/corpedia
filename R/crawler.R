get_and_xpath <- function(URL, XPath) {
  dom <- get(URL)
  xpath(dom, XPath)
}

get <- function(URL) {
  print(paste("Getting", URL))
  htmlParse(
    getURL(URL,
           followlocation = TRUE,
           httpheader=c("User-Agent" = "Harmless Crawler, <antoinogan@gmail.com>")))
}

get_all <- function(URLs) {
  doms <- c()
  for(URL in URLs) {
    dom <- get(URL)
    doms <- append(doms,dom)
  }
  names(doms) <- unlist(URLs)
  return(doms)
}

xpath <- function(dom,XPath) {
  getNodeSet(dom,XPath)
}

get_attrs <- function(dom,XPath) {
  sapply(xpath(dom,XPath),xmlAttrs)
}

get_first_attr <- function(dom,XPath) {
  sapply(xpath(dom,XPath),function(x){x[[1]]})
}

get_elt <- function(dom,XPath) {
  sapply(xpath(dom,XPath),xmlValue)
}

get_wiki_lang_link <- function(dom,lang) {
  get_first_attr(dom,paste('//li[@class="interwiki-',lang,'"]/a/@href',sep=''))
}

get_wiki_body_content <- function(dom) {
  get_elt(dom,'//div[@id="bodyContent"]//*[not(child::p)]//text()')
}

get_wiki_links <- function(URL,dom) {
  # Get all 'html:a' elements from the bodyContent div
  anchors <- get_attrs(dom,'//div[@id="bodyContent"]//a[not(contains(@class,"nofollow")) and not(contains(@class,"extiw"))]')
  # Get all only those anchors for which the href attribute does not point to an internal
  # MediaWiki page or category page.
  wiki_anchors <- anchors[which(sapply(anchors, function(x) {grepl("/wiki/[[:alnum:]_]+$",x['href'])}))]
  wiki_links <- sapply(wiki_anchors, function(x) { absolute_wiki_url(x['href'],URL) })
  return(wiki_links)
}

absolute_wiki_url <- function(URL,cur_page) {
  sp <- strsplit(cur_page,'/')[[1]]
  if(substr(URL,1,2) == '//') {
    paste('http:',URL,sep="")
  } else if(substr(URL,1,1) == '/') {
    paste('http://',sp[3],URL,sep="")
  } else if(substr(URL,1,1) == '#') {
    paste(cur_page,URL,sep="")
  } else {
    URL
  }
}

politicus_filter <- function(dom) {
  if (length(xpath(dom,'//div[@id="mw-normal-catlinks"]//a[contains(./text(),"politicus")]')) > 0) {
    TRUE
  } else {
    FALSE
  }
}

politicus_of_kabinet_filter <- function(dom) {
  if (length(xpath(dom,'//div[@id="mw-normal-catlinks"]//a[contains(./text(),"politicus")]')) > 0 || length(xpath(dom,'//div[@id="mw-normal-catlinks"]//a[contains(./text(),"Nederlands kabinet")]')) > 0) {
    TRUE
  } else {
    FALSE
  }
}

living_people_filter <- function(dom) {
  if (length(xpath(dom,'//a[./text()="Living people"]')) > 0) {
    TRUE
  } else {
    FALSE
  }
}

living_people_or_movie_filter <- function(dom) {
  if (length(xpath(dom,'//div[@id="mw-normal-catlinks"]//a[./text()="Living people"]')) > 0 || length(xpath(dom,'//div[@id="mw-normal-catlinks"]//a[contains(./text(),"movie")]')) > 0) {
    TRUE
  } else {
    FALSE
  }
}

movie_filter <- function(dom) {
  if (length(xpath(dom,'//div[@id="mw-normal-catlinks"]//a[contains(./text(),"movie")]')) > 0) {
    TRUE
  } else {
    FALSE
  }
}

crawl_depth_first_aux <- function(URL,step_limit,selection,done) {
  if (step_limit < 1) {
    return(list(edges=c(),done=done))
  } else {
    if (!(URL %in% done)) {
      done <- unique(append(done,c(URL)))
      try( {
          dom <- get(URL)
          if (!is.null(selection) && !sapply(c(dom),selection)) {
            print(paste("not following",URL))
            return(list(edges=c(),done=done))
          } else {
            print(paste("following",URL))
            URL_name <- gsub('http://.*/','',URL)
            links <- unique(get_wiki_links(URL,dom))
            edges <- c()
            for (l in links) {
              l_name <- gsub('http://.*/','',l)
              edges <- append(edges,c(URL_name,l_name))
              results <- crawl_depth_first_aux(l,step_limit - 1,selection,done)
              edges <- append(edges,results$edges)
              done <- unique(append(done,results$done))
            }
            return(list(edges=edges,done=done))
          }
        }, silent=FALSE)
    } else {
      print(paste("already visited",URL))
    }
    return(list(edges=c(),done=done))
  }
}

crawl <- function(URL, step_limit,selection=NULL) {
  graph.edgelist(t(matrix(crawl_depth_first_aux(URL,step_limit,selection,done=c())$edges,nrow=2)))
}

save.graph <- function(g, filename) {
  edgelist <- get.edgelist(g)
  dimnames(edgelist) <- list(c(),c("Source","Target"))
  print(paste("Saving to: ",filename,".csv",sep=""))
  write.csv(edgelist, file=paste(filename,".csv",sep=""))
  print("Done")
}


