







# Hugo W3 SIMPLE

[Demo](https://themes.gohugo.io/theme/hugo-w3-simple/) | [Live](https://jesselau.com/) | [中文说明](https://github.com/jesselau76/hugo-w3-simple/blob/master/README-zh.md)


**Hugo W3 Simple** is a Hugo theme written by [Jesse Lau](https://jesselau.com) . The main motivation for writing this theme was to provide a really minimal theme with W3 CSS included. 


## Features:

 - W3.css - small and fast
 - Responsive
 - W3 code color js - small and fast code highlight solution. 
 - Twitter Card
 - Google Analytics
 - Disqus or isso comment system
 - One Signal Push
 - Social share & Bookmark bar
 - Shortcodes:Info, Warning, Colorcode, quote.  [Demo Here](https://jesselau.com/w3-simple-shortcodes-demo/)
 - Sphinx or Manticore search supported. Output xml file which can be indexed by sphinx and manticore search. Example site can not include search function because it need server-side setting. [Live Search function Demo Here](https://jesselau.com/search/)
 - Table of content and back to top button on scroll.
 - Alllist, Grid style and simple list style.
 - All posts in one page. With a filterable, sortable and responsive table powered by w3.js. [Live Demo Here.](https://jesselau.com/en/allposts/)
 - Google translate.
 - Social icon.
 - Multilingual.
 - Glowing style logo
 - Scroll indicator
 - Google Adsense autoads
 - Detect adblock and display allow ads notice on scrolling over 2000px if the users have adblock installed.
 - Cookie law support.
 - Lazy Load images -  loading images on websites asynchronously — that is, after the above-the-fold content is fully loaded, or even conditionally, only when they appear in the browser's viewport. This means that if users don't scroll all the way down, images placed at the bottom of the page won't even be loaded. Enable this function to make the page load very fast if it has lots of images. [Live Demo Here](https://jesselau.com/21-wordpress-plugins-activated-in-my-website/). This page includes 21 images. Before lazy load the page size is 854kb and load time is 3.5s.![Before lazy load](https://raw.githubusercontent.com/jesselau76/hugo-w3-simple/master/images/beforelazyload.png)After lazy load the page size is 524kb and load time is 2.8s
![After lazy load](https://raw.githubusercontent.com/jesselau76/hugo-w3-simple/master/images/afterlazyload.png)

 ## Screenshot
### Grid style 
![HUGO W3 SIMPLE](https://raw.githubusercontent.com/jesselau76/hugo-w3-simple/master/images/tn.png)
### Simple list style
 
![HUGO W3 SIMPLE DEVICE](https://raw.githubusercontent.com/jesselau76/hugo-w3-simple/master/images/device.png)
 
 ### All posts in one page. With a filterable, sortable and responsive table
 
![HUGO W3 SIMPLE all posts](https://raw.githubusercontent.com/jesselau76/hugo-w3-simple/master/images/allposts.png)
 

## Install:
 ```bash
 cd themes
 git clone https://github.com/jesselau76/hugo-w3-simple.git
 ```
 
## How to configure?
 It is very simple. You can edit the example **config. toml** file from examplesite.
 

## Multilingual
Hugo W3 Simple supports multilingual. example site includes both English and Chinese version. You should put your posts on `./content/{langcode}/` folder.

## Search Page

To use search function, you need create `./content/{langcode}/search/index.md` file. Please see exampleSite folder.

## All posts

To use all posts function, you need create `./content/{langcode}/allposts/_index.md` file. Please see exampleSite folder.


## Favicon

In order to customize the favicon you need to place the `favicon.ico` file in the static folder at the root of your site, which will overwrite those files in the themes/even/static/ folder.


## Update Theme

```bash
cd  themes/hugo-w3-simple/
git pull
```

## License

Released under the [MIT](https://github.com/jesselau76/hugo-w3-simple/blob/master/LICENSE) License.

## Acknowledgements

- [Hugo Xmin](https://github.com/yihui/hugo-xmin)
- [Even](https://github.com/olOwOlo/hugo-theme-even)
- [Mediumish](https://github.com/lgaida/mediumish-gohugo-theme)





