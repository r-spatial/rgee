









# Hugo W3 SIMPLE


**Hugo W3 Simple** 由 [Jesse Lau](https://jesselau.com)创作。根由是原本打算做一个极简化的基于W3 CSS的Wordpress模板，却无意中搜索到了HUGO，从而被HUGO圈粉，转Wordpress到HUGO，顺便做了这个模板，并逐步改进。 


## 主要特点:

 - 采用W3.css - 同类CSS框架文件最小的，最小故最快。
 - 自适应移动设备
 - W3 code color js - 也是小而快的代码高亮模块。 
 - Twitter Card
 - Google Analytics
 - Disqus 或isso评论系统，[isso中文安装教程](https://jesselau.com/install-isso-nginx-hugo/)
 - One Signal Push
 - 书签及共享按钮条，大屏幕时常位于左侧，中小屏幕位于帖子下方。
 - 当前版本四种快捷方式:Info, Warning, Colorcode, quote.  [演示](https://jesselau.com/hugo-w3-simple-shortcodes-demo/
 - Sphinx 或者Manticore search全文本搜索支持. 直接生成可被Sphinx 或者Manticore search检索的XML文件。[运行演示](https://jesselau.com/search/)
 - 滑屏时自动显示文章目录、进度条指示和回到顶端按钮
 - 全部列表式风格、网格化风格和极简化风格，三种风格可切换。
 - 全部帖子可集中于一页，并带表格排序、标题过滤功能. [运行演示](https://jesselau.com/allposts/)
 - Google翻译.
 - 社交按钮.
 - 支持多语言.
 - 辉光式LOGO，可选
 - Cookie设置提醒，根据欧盟法律需显示提示站点采用cookie。
 - Google Adsense autoads
 - 反反广告插件支持，滑屏2000px若检测到用户装有adblock，则弹出提醒窗口。
 - Lazy Load图片 -  异步加载网站上的图像 - 也就是说先用一个极小的图像文件占住位置，只有当用户滑动屏幕到该图片出现的位置时，才调用真实的图片。 如果页面有很多图片，启用此功能可以使页面加载速度非常快。 [实例演示](https://jesselau.com/21-wordpress-plugins-activated-in-my-website/). 这个网页含有21个图片。不用lazy load前文件大小为854kb，加载时间3.5s.![Before lazy load](https://raw.githubusercontent.com/jesselau76/hugo-w3-simple/master/images/beforelazyload.png)使用lazy load后文件大小为524kb，加载时间就只有2.8s
![After lazy load](https://raw.githubusercontent.com/jesselau76/hugo-w3-simple/master/images/afterlazyload.png)

 ## 屏幕截图
### 网格化风格 
![HUGO W3 SIMPLE](https://raw.githubusercontent.com/jesselau76/hugo-w3-simple/master/images/tn.png)
### 极简化风格
 
![HUGO W3 SIMPLE DEVICE](https://raw.githubusercontent.com/jesselau76/hugo-w3-simple/master/images/device.png)
 
 ### 全部帖子在一页，有根据帖子发布时间、字数统计排序
 
![HUGO W3 SIMPLE all posts](https://raw.githubusercontent.com/jesselau76/hugo-w3-simple/master/images/allposts.png)
 

## 安装:
 ```bash
 cd themes
 git clone https://github.com/jesselau76/hugo-w3-simple.git
 ```
 
## 如何配置?
请将examplesite的 **config. toml** 文件拷贝并参照配置。
 

## 多语言站点
Hugo W3 Simple支持多语言站点. example site目录包括了英文和中文两者。可参照examplesite的 **config. toml** 文件配置

## 搜索功能

如果要用sphinx或manticoresearch来实现站内搜索功能。需要建立一个 `./content/{langcode}/search/index.md` 文件。

<b>注意</b>：为安全考虑，不建议服务器支持HTML文件运行PHP语言，请将Hugo生成的search/index.html更名为index.php再上传服务器。

## 全部帖子页面

要生成全部帖子页面，需要创建 `./content/{langcode}/allposts/_index.md` 文件。可参照exampleSite内文件。



## 更新主题

本主题会经常更新，故此用下列命令可快速更新到最新版本

```bash
cd  themes/hugo-w3-simple/
git pull
```

## 版权

采用 [MIT](https://github.com/jesselau76/hugo-w3-simple/blob/master/LICENSE) License.

## 致谢
从下列主题中学到不少Hugo做主题的经验，特此致谢：

- [Hugo Xmin](https://github.com/yihui/hugo-xmin)
- [Even](https://github.com/olOwOlo/hugo-theme-even)
- [Mediumish](https://github.com/lgaida/mediumish-gohugo-theme)






