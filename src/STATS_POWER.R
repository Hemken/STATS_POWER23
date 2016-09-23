#/***********************************************************************
# * Licensed Materials - Property of IBM 
# *
# * IBM SPSS Products: Statistics Common
# *
# * (C) Copyright IBM Corp. 2016
# *
# * US Government Users Restricted Rights - Use, duplication or disclosure
# * restricted by GSA ADP Schedule Contract with IBM Corp. 
# ************************************************************************/

# author__ = "IBM SPSS, JKP"
# version__ = "1.0.0"

# History
# 15-sep-2016 Original Version




gtxt <- function(...) {
    return(gettext(...,domain="STATS_POWER"))
}

gtxtf <- function(...) {
    return(gettextf(...,domain="STATS_POWER"))
}
# method name translation mapping
# names should probably be left as these words but might be
# transliterated

dopower = function(ptype, power=NULL, nobs=NULL, nobs1=NULL, size=NULL, sig=NULL,
    n=NULL, ttype=NULL, numgroups=NULL, r=NULL, df=NULL, u=NULL, v=NULL,
    althypot="twosided") {

  procname=gtxt("Power Calculations")
  warningsprocname = gtxt("Power Calculations: Warnings")
  omsid="STATSPOWER"
  warns = Warn(procname=warningsprocname,omsid=omsid)
  tryCatch(library(pwr), error=function(e){
    warns$warn(gtxtf("The R %s package is required but could not be loaded.", "pwr"),dostop=TRUE)
}
)

# This list maps the type of calculation to the implementing function
powerfuncsmap = list("ttestunequaln"=pwr.t2n.test, "ttestequaln"=pwr.t.test,
    "proporunequaln"=pwr.2p2n.test, "proporequaln"=pwr.2p.test,
    "anova"=pwr.anova.test, "correlation"=pwr.r.test, "chisquared"=pwr.chisq.test,
    "linear"=pwr.f2.test, "normalmean"=pwr.norm.test, "propor"=pwr.p.test
    )
# These list map Statistics keyword values to the R function parameter names
althypotmap = list("twosided"="two.sided", "greaterthan"="greater", "lessthan"="less")
ttypemap = list("twosample"="two.sample", "onesample"="one.sample", "paired"="paired")



    
    setuplocalization("STATS_POWER")
    
    # A warnings proc name is associated with the regular output
    # (and the same omsid), because warnings/errors may appear in
    # a separate procedure block following the regular output


    # set up the call to the estimation function based on the type of calculation
    # arglist will be the actual arguments to the call
    # items is the set of parameters exactly one of which must be NULL
    
    # due to a bug in the 24.0.0.1 CDB, it is necessary to allow a phantom
    # extra parameter for ttype, which will be ignored.  The legitimate
    # value will be first, and if ttype is not used, the value will not be used.
    ttype = ttype[[1]]
    
    if (ptype == "ttestunequaln") {
      arglist = list(n1=nobs, n2=nobs1, d=size, sig.level=sig, power=power, 
          alternative=althypotmap[[althypot]])
      items = list(power=power, n1=nobs, n2=nobs1, d=size, sig.level=sig)
      
    } else if (ptype == "ttestequaln") {
      arglist = list(n=nobs, d=size, sig.level=sig, power=power,
          alternative=althypotmap[[althypot]], type=ttypemap[[ttype]])
      items = list( power=power, n=nobs, d=size, sig.level=sig)
      
    } else if (ptype == "proporequaln") {
      arglist = list(n=nobs, h=size, sig.level=sig, power=power, 
                     alternative=althypotmap[[althypot]])
      items = list(power=power, n=nobs,  h=size, sig.level=sig)
      
    } else if (ptype == "proporunequaln") {
      arglist = list(n1=nobs, n2=nobs1, h=size, sig.level=sig, power=power, 
                     alternative=althypotmap[[althypot]])
      items = list(power=power, n1=nobs, n2=nobs1, h=size, sig.level=sig)
      
    } else if (ptype=="anova") {
      arglist = list(k=numgroups, n=nobs, f=size, sig.level=sig, power=power)
      items = list(k=numgroups, power=power, n=nobs,f=size, sig.level=sig)
      
    } else if (ptype == "correlation") {
        arglist = list(r=r, n=nobs, sig.level=sig, power=power,
             alternative=althypotmap[[althypot]])
        items = list(r=r, power=power, n=nobs, sig.level=sig)
        
    } else if (ptype == "chisquared") {
      arglist = list(w=size, N=nobs, df=df, sig.level=sig, power=power)
      items = list(w=size, power=power, n=nobs, df=df, sig.level=sig)
      
    } else if (ptype == "linear") {
      arglist = list(u=u, v=v, f2=size, sig.level=sig, power=power)
      items = list(u=u, v=v, f2=size, power=power, sig.level=sig)
      
    } else if (ptype == "normalmean") {
      arglist = list(d=size, n=nobs, sig.level=sig, power=power,
         alternative=althypotmap[[althypot]])
      items = list(d=size, n=nobs, power=power, sig.level=sig)
      
    } else if (ptype == "propor") {
      arglist = list(h=size, n=nobs, sig.level=sig, power=power,
           alternative=althypotmap[[althypot]])
      items = list(h=size, n=nobs, power=power, sig.level=sig)
    }

    # check to see if any legal keywords not used for this particular
    # function were specified.  Have to work around phony ttype=="na case

    sclen = length(sys.call())
    if (ttype == "na") {
      sclen = sclen - 1
    }
    if (sclen - 2 > length(arglist)) {
      warns$warn(gtxt("Unused parameters were specified and will be ignored"), dostop=FALSE)
    }
    
    specs = validate(ptype, items, warns)
    resx = calcres(specs, items, arglist, powerfuncsmap, ptype, warns)
    res = resx[[1]]
    numrows = resx[[2]]
    displayresults(ptype, althypot, arglist, res, numrows, procname, omsid, warns)
}
    

calcres <- function(specs, items, arglist, powerfuncsmap, ptype, warns) {
  #  calculate simple or iterated result and return combined result4
  # Structure of result will vary according to the type of test
  
  res = NULL
  failcount = 0
  if (specs[2] != 0) {  # iterated result required?
    z = specs[[2]]  # name of iterated item
    itemslist = seq(items[[z]][[1]], items[[z]][[2]], items[[z]][[3]])
    numrows = length(itemslist)
  } else {
    numrows = 1
    itemslist = list(999)
    z = NULL
  }
    for (value in itemslist) {
      if (numrows > 1) {
        arglist[z] = value
      }

      res1 = tryCatch(do.call(powerfuncsmap[[ptype]], arglist),
                     error=function(e) {if (e$message == "f() values at end points not of opposite sign") {
                       return(NULL)
                     } else {
                       warns$warn(e, dostop=TRUE)
                     }
                     }
      )
      if (is.null(res1)) {
        failcount = failcount + 1
      } else {
        # merge results
        if (is.null(res)) {
          res = res1
          namelist = names(res1)
        } else {
          res = Map(c, res, res1)
        }
      }
    }
    if (is.null(res)) {
      warns$warn(gtxt("No results could be computed.  Please try different values"), dostop=TRUE)
    }
    if (failcount > 0) {
      numrows = numrows - failcount
      warns$warn(gtxt("Results could not be computed for some parameter values"), dostop=FALSE)
    }
    names(res) = namelist
    return(list(res, numrows, z))
}

# mapping of syntax keywords to translatable text for the table title
testtypes = list("ttestunequaln"= gtxt("Two-Sample T Test with Unequal N"),
    "ttestequaln"=gtxt("Two-sample T Test, Equal N"),
    "proporequaln"=gtxt("Two Proportions, Equal N"),"proporunequaln"=gtxt("Two Proportions, Unequal N"),
    "anova"=gtxt("ANOVA"), "correlation"=gtxt("Correlation"), "chisquared"= gtxt("Chisquared"),
    "linear"=gtxt("General Linear Model"), "normalmean"=gtxt("Mean of Normal Distribution"),
    "propor"=gtxt("One-Sample Proportion")
)

althyp = list("two.sided"= gtxt("Two Sided"), "greater"= gtxt("Greater Than"), "less"= gtxt("Less Than "))

# mapping of names in the result object to translatable titles.  If a name in that object
# is not listed here, it is suppressed
resnames = list(n=gtxt("Number of Observations"), d=gtxt("Effect Size"), h=gtxt("Effect Size"),
          sig.level=gtxt("Significance Level"), power=gtxt("Power"), type=gtxt("Type of T Test"),
          n1=gtxt("Number of Observations(1)"), n2=gtxt("Number of Observations(2)"),
          k=gtxt("Number of Groups"), r="Correlation Coefficient", N=gtxt("Number of Observations"),
          w=gtxt("Effect Size"), df=gtxt("D.F"), u=gtxt("Numerator D.F."), v=gtxt("Denominator D. F."),
          d=gtxt("Effect Size"), f=gtxt("Effect Size"), f2=gtxt("Effect Size")
          )

displayresults = function(ptype, althypot, arglist, res, numrows, procname, omsid, warns) {
  # display a table of results and, if more than one set, a plot
  # ptype is the test tpe
  # althypot is the alternative
  # arglist is the set of parameters passed to the estimation function
  # res is the result from that function
  # numrows is the number of sets of parameter values
  # procname, omsid, and warns are OMS identifiers and the warnings object
  
    StartProcedure(procname, omsid)

    title = gtxtf("Power Calculations: %s", testtypes[[ptype]])
    
    # Find the column names that will appear (varies by test type)
    # Then construct rows repeating scalar parameters for each iterated
    # parameter (if any)
    columnnames = c()
    ncol = 0
    for (name in names(res)) {
      if (name %in% names(resnames)) {
        columnnames[[name]] = resnames[[name]]
        ncol = ncol+1
      }
    }

    pcols= c()
    pcolkt = 1
    df = data.frame(row.names=1:numrows)
    ncol = 1
    for (name in names(res)) {
      if (name %in% names(resnames)) {
        for (i in 1:numrows) {
          ##if (length(res[[name]]) == 1) {
          if (length(unique(res[[name]])) == 1) {
            df[i, ncol] = res[[name]][[1]]
          } else {
            df[i, ncol] = res[[name]][i]
            pcols[[pcolkt]] = ncol
            pcolkt = pcolkt + 1
          }
        }
        ncol = ncol + 1
      }

    }
    caption=ifelse(!is.null(res$alternative), 
        gtxtf("Alternative Hypothesis: %s", althyp[[res$alternative[[1]]]]), "")
    caption = gtxtf("%s\n%s\nResults from package pwr, version: %s",
        caption, res$method,
        as.character(packageVersion("pwr"))
        )
    pcols = unique(pcols)
    
    names(df) = columnnames

    spsspivottable.Display(df, title=title,
        templateName="POWER",
        outline=gtxt("Power Calculations"),
        caption=caption
    )

    if (numrows > 1) {
      pars = par(lwd=1.5, pch=16, cex=1.2)
      plot(df[pcols], type='b', main=gtxt("Power Calculations"))
      grid(col="gray", lty="dotted")
      par(pars)
    }
    # Force any non-fatal, queued messages to be displayed
    warns$display()

    spsspkg.EndProcedure()
}



validate = function(ptype, items, warns) {
  # check statistics list and return index of item to calculate
  # and name of iterated item or NULL
  
  # items is a list of lists, one of which must be NULL and
  # one of which may be a triple
  
  nullcount = 0
  itercount = 0
  iteritem = 0

  for (item in 1:length(items)) {
    if (is.null(items[[item]])) {
      nullcount = nullcount + 1
      tofind = item
      tofindname = names(items)[item]
    } else {
      len = length(items[[item]])
      if (len == 3) {
              itercount = itercount + 1
              iteritem = names(items)[item]
              iteritemnumber = item
      } else if (len != 1) {
          warns$warn(gtxtf("Invalid length for statistic: %s", paste(items[[item]], collapse=" ")), dostop=TRUE)
      }
    }
  }
    if (nullcount != 1) {
      warns$warn(gtxt("Exactly one item must be omitted from the statistics list"), dostop=TRUE)
    }
    if (itercount > 1) {
      warns$warn(gtxt("Only one item may be iterated over"), dostop=TRUE)
    }

  return(c(tofind, iteritem, tofindname))
}

Warn = function(procname, omsid) {
    # constructor (sort of) for message management
    lcl = list(
        procname=procname,
        omsid=omsid,
        msglist = list(),  # accumulate messages
        msgnum = 0
    )
    # This line is the key to this approach
    lcl = list2env(lcl) # makes this list into an environment

    lcl$warn = function(msg=NULL, dostop=FALSE, inproc=FALSE) {
        # Accumulate messages and, if dostop or no message, display all
        # messages and end procedure state
        # If dostop, issue a stop.

        if (!is.null(msg)) { # accumulate message
            assign("msgnum", lcl$msgnum + 1, envir=lcl)
            # There seems to be no way to update an object, only replace it
            m = lcl$msglist
            m[[lcl$msgnum]] = msg
            assign("msglist", m, envir=lcl)
        } 

        if (is.null(msg) || dostop) {
            lcl$display(inproc)  # display messages and end procedure state
            if (dostop) {
                stop(gtxt("End of procedure"), call.=FALSE)  # may result in dangling error text
            }
        }
    }
    
    lcl$display = function(inproc=FALSE) {
        # display any accumulated messages as a warnings table or as prints
        # and end procedure state, if any

        if (lcl$msgnum == 0) {   # nothing to display
            if (inproc) {
                spsspkg.EndProcedure()
            }
        } else {
            if (!inproc) {
                procok =tryCatch({
                    StartProcedure(lcl$procname, lcl$omsid)
                    TRUE
                    },
                    error = function(e) {
                        FALSE
                    }
                )
            } else {
                procok = TRUE
            }
            if (procok) {  # build and display a Warnings table if we can
                table = spss.BasePivotTable("Warnings ","Warnings") # do not translate this
                rowdim = BasePivotTable.Append(table,Dimension.Place.row, 
                    gtxt("Message Number"), hideName = FALSE,hideLabels = FALSE)

                for (i in 1:lcl$msgnum) {
                    rowcategory = spss.CellText.String(as.character(i))
                    BasePivotTable.SetCategories(table,rowdim,rowcategory)
                    BasePivotTable.SetCellValue(table,rowcategory, 
                        spss.CellText.String(lcl$msglist[[i]]))
                }
                spsspkg.EndProcedure()   # implies display
            } else { # can't produce a table
                for (i in 1:lcl$msgnum) {
                    print(lcl$msglist[[i]])
                }
            }
        }
    }
    return(lcl)
}


# localization initialization
setuplocalization = function(domain) {
    # find and bind translation file names
    # domain is the root name of the extension command .R file, e.g., "SPSSINC_BREUSCH_PAGAN"
    # This would be bound to root location/SPSSINC_BREUSCH_PAGAN/lang

    fpath = Find(file.exists, file.path(.libPaths(), paste(domain, ".R", sep="")))
    bindtextdomain(domain, file.path(dirname(fpath), domain, "lang"))
} 
# override for api to account for extra parameter in V19 and beyond
StartProcedure <- function(procname, omsid) {
    if (substr(spsspkg.GetSPSSVersion(),1, 2) >= 19) {
        spsspkg.StartProcedure(procname, omsid)
    }
    else {
        spsspkg.StartProcedure(omsid)
    }
}



Run = function(args) {
    #Execute the STATS POWER command

    cmdname = args[[1]]
    args = args[[2]]
    oobj = spsspkg.Syntax(list(
        spsspkg.Template("TYPE", subc="",  ktype="str", var="ptype",
              vallist=list("ttestunequaln", "ttestequaln",
                  "proporunequaln", "proporequaln",
                  "anova", "correlation", "chisquared", "linear", "normalmean",
                  "propor")
              ),
        spsspkg.Template("POWER", subc="", ktype="float", var="power", islist=TRUE),
        spsspkg.Template("NOBS", subc="", ktype="float", var="nobs", islist=TRUE),
        spsspkg.Template("NOBS1", subc="", ktype="float", var="nobs1", islist=TRUE),
        spsspkg.Template("NUMGROUPS", subc="", ktype="int", var="numgroups", islist=TRUE,
              vallist=list(2)),
        spsspkg.Template("EFFECTSIZE", subc="", ktype="float", var="size", islist=TRUE),
        spsspkg.Template("R", subc="", ktype="float", var="r", islist=TRUE),
        spsspkg.Template("DF", subc="", ktype="float", var="df", islist=TRUE,
            vallist=list(1)),
        spsspkg.Template("U", subc="", ktype="float", var="u", islist=TRUE,
            vallist=list(1)),
        spsspkg.Template("V", subc="", ktype="float", var="v", islist=TRUE,
            vallist=list(1)),
        spsspkg.Template("SIG", subc="", ktype="float", var="sig", islist=TRUE),
        spsspkg.Template("ALTHYPOT", subc="", ktype="str", var="althypot",
            vallist=list("twosided", "greaterthan", "lessthan")),
        spsspkg.Template("TTYPE", subc="", ktype="str", var="ttype", islist=TRUE,
            vallist = list("twosample", "onesample", "paired", "na"))
    ))

    # A HELP subcommand overrides all else
    if ("HELP" %in% attr(args,"names")) {
        #writeLines(helptext)
        helper(cmdname)
    }
    else {
        res <- spsspkg.processcmd(oobj, args, "dopower")
    }
}

helper = function(cmdname) {
    # find the html help file and display in the default browser
    # cmdname may have blanks that need to be converted to _ to match the file
    
    fn = gsub(" ", "_", cmdname, fixed=TRUE)
    thefile = Find(file.exists, file.path(.libPaths(), fn, "markdown.html"))
    if (is.null(thefile)) {
        print("Help file not found")
    } else {
        browseURL(paste("file://", thefile, sep=""))
    }
}
if (exists("spsspkg.helper")) {
assign("helper", spsspkg.helper)
}