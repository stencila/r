Running Stencila via RStudio
====

**Hello and welcome!** :tada:

This guide will help you set up RStudio to run Stencila locally in the browser, without assuming that you're super comfortable with RStudio. This is a fun way to explore data and learn more about what R can do with a friendly Stencila interface. In this document, we will run through the steps to get started and identify a few trouble-shooting strategies.

:bug: As software developers, bugs are a part of our life. This integration is new and experimental and is provided mainly for testing. So we want to know if you find a bug! Please don't hesitate to reach out if you are having trouble by creating a new [issue](https://github.com/stencila/r/issues). 

👋🏽 We also want to know if you find it useful! If you want to learn more about Stencila or if you have an idea for a feature, join us on the [community forum](https://community.stenci.la/).

More support for setting up RStudio can be found [here](https://jennybc.github.io/2014-05-12-ubc/r-setup.html) and [here](https://support.rstudio.com/hc/en-us/categories/200035113-Documentation)

--
Step 1. **Install the Stencila R package** by running this code in Rstudio. If you haven't used `devtools` before, you will need to install that package first:

```
install.packages('devtools')
```

Then, install the Stencila R package directly from the Github repository:

```
devtools::install_github("stencila/r", ref="master")
```

If you have problems installing `stencila/r` using `devtools`, try using one of the [pre-built Stencila R releases](https://github.com/stencila/r/releases).


Step 2. **Set your working directory** to the file path where you've downloaded the examples you want to work with. You can do this by typing in the console with the command ```setwd``` or through the interface, read more about workspaces and directories [here](https://support.rstudio.com/hc/en-us/articles/200711843-Working-Directories-and-Workspaces). For example, I'll be working with a file called `Hello-Nokome.rmd`, located in the directory `Friendly-Intro-to-R`:
```
setwd("~/Documents/Friendly-Intro-to-R")
```
Step 3. Now it's time to **open your file!** We are running a bit of code first that turns off the authorization, as this was buggy in our earlier trials. Run the following code, with your file path, in Rstudio:
```
stencila:::start(authorization=FALSE)
stencila:::open("~/Documents/Friendly-Intro-to-R/Hello-Nokome.rmd")
```
Alternately, open the Markdown file you wish to view in RStudio, click the "Addins" menu item and select the "Open with Stencila" option. This should also pop up right away in the browser.

Step 4. **Use Stencila and R in your browser!** Stencila should pop up in a browser window right away. The green blinking dot means Stencila is loading in the browser. It will then connect to your R session and load the document.

Step 5. **Saving or printing your work.**
Windows: `Cntrl+S` will allow you to save the markdown file you were editing in the browser.
Mac: `Command+S` will save the markdown file you were editing in the browser in RStudio. Check the RStudio console and you should see your updated file.


Step 6. **Stop your Stencila session** by typing:
```
stencila:::stop()
```

**Errors**

We hope you don't run into errors, but if you do try quitting your browser session, clearing the cache, or a different browser.

**Alternatives**

For a less experimental way to use Stencila with R, please download [Stencila Desktop](https://github.com/stencila/desktop). Stencila Desktop provides a more stable platform which allows you to connect to multiple languages.
