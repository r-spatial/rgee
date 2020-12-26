---
author: admin
date: 2018-11-09T07:26:15+13:00
draft: false
title: Hugo W3 Simple Theme Shortcodes Demo
type: post
url: /w3-simple-shortcodes-demo/
w3codecolor: true
categories:
- Website Programming
tags:
- hugo
---


This article is about the demo of these shortcodes.

<hr>

## Info Shortcodes
We can easily display info with this code below on markdown file
{{< colorcode title="Code for info shortcodes">}}  
{{&lt; info  &gt;}}<br>  
Blue often indicates a neutral informative change or action. <br> 
{{&lt;/info &gt;}}  
{{< /colorcode >}}  
It will display default title with `Information!`:  
{{< info  >}}  
Blue often indicates a neutral informative change or action. 
{{< /info >}}    

You can set title with the following code:

{{< colorcode title="Code for info shortcodes with title">}}  
{{&lt; info  title="W3.CSS is a CSS Framework!"&gt;}} <br> 
W3.CSS is a modern CSS framework with built-in responsiveness. It supports responsive mobile first design by default, and it is smaller and faster than similar CSS frameworks. <br> 
{{&lt; /info &gt;}}  
{{< /colorcode >}}  
It will display the title with your set:  
{{< info  title="W3.CSS is a CSS Framework!">}}  
W3.CSS is a modern CSS framework with built-in responsiveness. It supports responsive mobile first design by default, and it is smaller and faster than similar CSS frameworks. 
{{< /info >}}  

<hr>

## Warning Shortcodes
We can easily display warning with this code below on markdown file
{{< colorcode title="Code for warning shortcodes">}}  
{{&lt; warning  &gt;}}<br>  
Yellow often indicates a warning that might need attention. <br> 
{{&lt;/warning &gt;}}  
{{< /colorcode >}}  
It will display default title with `Warning!`:  
{{< warning  >}}  
Yellow often indicates a warning that might need attention. 
{{< /warning >}}    

You can set title with the following code:

{{< colorcode title="Code for warning shortcodes with title">}}  
{{&lt; warning title="Do Not Bypass This Theme!"&gt;}} <br> 
W3-simple is a Hugo theme powered by W3.css! <br> 
{{&lt; /warning &gt;}}  
{{< /colorcode >}}  
It will display the title with your set:  
{{< warning  title="Do Not Bypass This Theme!">}}  
W3-simple is a Hugo theme powered by W3.css! 
{{< /warning >}}  

<hr>

## Colorcode shortcodes
W3-simple theme use w3colorcode.js to display code colors.    

To use this function, you should add `w3codecolor: true` to your post.   

Although w3colorcode.js is much smaller than other js, but it is still more than 20kb. So I do not want every page include this js file if no need.  

w3colorcode.js is also built by  [W3school](https://www.w3schools.com/w3css/w3css_code.asp)  

It supports `html`, `js`, `java`, `css`, `sql`, `python` at this moment.   

I just changed a little to add support to `bash` script.  

Colorcode shortcodes use `mode` and `title` to set up parameters. if not set, default color mode is `bash` and no title.  

{{< colorcode title="Code for default colorcode shortcodes">}}  
{{&lt; colorcode  &gt;}}<br>  
your code here<br> 
{{&lt;/colorcode &gt;}}  
{{< /colorcode >}}  
The demo for default colorcode shortcodes:
{{< colorcode >}}  
To install w3-simple theme, just use the command below<br>
cd themes<br>
git clone https://github.com/jesselau76/hugo-w3-simple.git<br> 
{{< /colorcode >}}  


{{< colorcode title="Code for html colorcode shortcodes">}}  
{{&lt; colorcode mode="htmlHigh" title="Demo for html color" &gt;}}<br>  
your code here<br> 
{{&lt;/colorcode &gt;}}  
{{< /colorcode >}}  

{{< colorcode mode="htmlHigh" title="Demo for html color" >}}  
&lt;!DOCTYPE html&gt;<br>&lt;html&gt;<br>
    &lt;title&gt;HTML Tutorial&lt;/title&gt;<br>
    &lt;body&gt;<br><br>
    &lt;h1&gt;This is a heading&lt;/h1&gt;<br>
    &lt;p&gt;This is a paragraph.&lt;/p&gt;<br><br>
    &lt;/body&gt;<br>
    &lt;/html&gt; 
{{< /colorcode >}}  


{{< colorcode title="Code for css colorcode shortcodes">}}  
{{&lt; colorcode mode="cssHigh" title="Demo for css color" &gt;}}<br>  
your code here<br> 
{{&lt;/colorcode &gt;}}  
{{< /colorcode >}}  

{{< colorcode mode="cssHigh" title="Demo for css color" >}}  
body {<br>
    background-color: #d0e4fe;<br>
    }<br><br>

    h1 {<br>
    color: orange;<br>
    text-align: center;<br>
    }<br><br>

    p {<br>
    font-family: "Times New Roman";<br>
    font-size: 20px;<br>
    }
{{< /colorcode >}}  


{{< colorcode title="Code for js colorcode shortcodes">}}  
{{&lt; colorcode mode="jsHigh" title="Demo for js color" &gt;}}<br>  
your code here<br> 
{{&lt;/colorcode &gt;}}  
{{< /colorcode >}}  

{{< colorcode mode="jsHigh" title="Demo for js color" >}}  
function w3CodeColor() {<br>
  var x, i, j, k, l, modes = ["html", "js", "java", "css", "sql", "python"];<br>
  if (!document.getElementsByClassName) {return;}<br>
  k = modes.length;<br>
  for (j = 0; j < k; j++) {<br>
    x = document.getElementsByClassName(modes[j] + "High");<br>
    l = x.length;<br>
    for (i = 0; i < l; i++) {<br>
      x[i].innerHTML = w3CodeColorize(x[i].innerHTML, modes[j]);<br>
    }<br>
  }<br>
}<br>
{{< /colorcode >}}  

{{< colorcode title="Code for sql colorcode shortcodes">}}  
{{&lt; colorcode mode="sqlHigh" title="Demo for sql color" &gt;}}<br>  
your code here<br> 
{{&lt;/colorcode &gt;}}  
{{< /colorcode >}}  

{{< colorcode mode="sqlHigh" title="Demo for sql color" >}}  
SELECT column1, column2, ...<br>  
FROM table_name<br>  
ORDER BY column1, column2, ... ASC|DESC; <br>  
{{< /colorcode >}}  

{{< colorcode title="Code for python colorcode shortcodes">}}  
{{&lt; colorcode mode="pythonHigh" title="Demo for python color" &gt;}}<br>  
your code here<br> 
{{&lt;/colorcode &gt;}}  
{{< /colorcode >}}  

{{< colorcode mode="pythonHigh" title="Demo for python color" >}}  
import json<br>

# some JSON:<br>
x =  '{ "name":"John", "age":30, "city":"New York"}'<br>

# parse x:<br>
y = json.loads(x)<br>

# the result is a Python dictionary:<br>
print(y["age"]) <br>
{{< /colorcode >}}  
<hr>

## quote shortcode

We can display quotes with shortcodes

{{< colorcode title="Code for quote shortcodes">}}  
{{&lt; quote  &gt;}}<br>  

     Make it as simple as possible, but not simpler.<br> 

{{&lt; /quote &gt;}}  
{{< /colorcode >}}  
It will display quote with default width 100% style and no author:  
{{< quote  >}}  

     Make it as simple as possible, but not simpler.

{{< /quote >}}  

You can also set width and author with shortcodes:
 {{< colorcode title="Code for quote shortcodes with % width">}}  
{{&lt; quote width="61.8%"  author="Albert Einstein" &gt;}}<br>  

     Make it as simple as possible, but not simpler.<br> 

{{&lt; /quote &gt;}}  
{{< /colorcode >}}  
{{< quote width="61.8%" author="Albert Einstein"  >}}  

     Make it as simple as possible, but not simpler.

{{< /quote >}}   

Or you can set px to width

{{< colorcode title="Code for quote shortcodes with px width">}}  
{{&lt; quote width="300px" author="Albert Einstein"  &gt;}}<br>  

     Make it as simple as possible, but not simpler.<br> 

{{&lt; /quote &gt;}}  
{{< /colorcode >}}  

{{< quote width="300px" author="Albert Einstein"  >}}  

     Make it as simple as possible, but not simpler.

{{< /quote >}}   

## Test first picture without thumbnail set
![Thanks to Unsplash](https://raw.githubusercontent.com/jesselau76/hugo-w3-simple/master/exampleSite/images/thumb-jad-limcaco-183877-unsplash.jpg)