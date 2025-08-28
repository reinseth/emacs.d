(defun my/reading-mode-hook  (buf)
  (switch-to-buffer buf)
  (visual-line-mode 1)
  (setq-local olivetti-body-width 100)
  (olivetti-mode 1)
  ;; (my/hide-cursor)
  (elfeed-show-refresh))

;; Source: https://jiewawa.me/2024/10/useful-emacs-commands-for-reading/
(defun my/hide-cursor ()
  (hl-line-mode 0)
  (setq-local cursor-type nil))

(defun my/show-cursor ()
  (hl-line-mode 1)
  (kill-local-variable 'cursor-type))

(defun my/toggle-cursor ()
  (interactive)
  (if (null cursor-type)
      (my/show-cursor)
    (my/hide-cursor)))

(use-package elfeed
  :ensure t
  :bind ("s-w" . elfeed)
  :config
  (setq elfeed-show-entry-switch #'my/reading-mode-hook)
  (setq elfeed-feeds
        '(
          ;; Web
          ("https://blog.codepen.io/feed/" web) ; Codepen blog
          ("http://chriscoyier.net/feed/" web) ; Chris Coyer's personal blog
          ("https://frontendfoc.us/rss/" web)  ; Weekly web news
          ("https://meyerweb.com/eric/thoughts/rss2/full" web) ; Eric Meyer, nuff said
          ("https://web.dev/feed.xml" web) ; Web platform news from Google
          ("https://bcd-watch.igalia.com/weekly/feed.rss" web) ; Weekly roundup of which features has made it to which browser
          ("https://bcd-watch.igalia.com/weekly-completed/feed.rss" web) ; Weekly roundup of web features that have become baseline
          ("http://bradfrost.com/blog/feed/" web) ; Web/design blog by Brad Frost

          ;; Dev - general
          ("http://einarwh.wordpress.com/feed/" dev) ; Einar W. Høst's blog
          ("https://blog.codinghorror.com/rss/" dev) ; Jeff Atwood, co-founder of Stack Overflow
          ("https://jvns.ca/atom.xml" dev git linux) ; Julie Evans
          ("https://www.kodemaker.no/atom.xml" dev)

          ;; Dev - clojure
          ("https://abhinavomprakash.com/index.xml" dev clojure)
          ("https://buttondown.com/tensegritics-curiosities/rss" dev clojure) ; Clojure/ClojureDart by Christiphe Grand and Babtiste Dupuch
          ("https://slipset.github.io/atom.xml" dev clojure) ; Erik Assum aka slipset
          ("https://explaining.software/rss.xml" dev clojure) ; Zach Tellman
          ("http://grishaev.me/feed.xml" dev clojure) ; Ivan Grishaev, author of the alternative, much faster postgres clojure driver
          ("http://www.metosin.fi/atom.xml" dev clojure)
          ("http://planet.clojure.in/atom.xml" dev clojure)
          ("http://blog.fogus.me/feed/" dev clojure) ; Fogus
          ("https://parenteser.mattilsynet.io/atom.xml" dev clojure)
          ("http://blog.cleancoder.com/atom.xml" dev clojure java) ; Uncle Bob
          ("https://thomascothran.tech/index.xml" dev clojure) ; Thomas Cothran, got some good insights on REST and the lack thereof
          ("https://tonsky.me/blog/atom.xml" dev clojure) ; Tonsky, author of FiraCode
          ("https://blog.agical.se/en/index.html" dev clojure) ; Agiacal, where Pez aka Peter Strömberg works (author of Calva)
          ("https://sophiebos.io" dev clojure) ; Sopie Bosio, works for Ardoq

          ;; Dev - js/ts/css
          ("https://infrequently.org/feed/" dev js) ; Alex Russel
          ("https://kurtextrem.de/rss.xml" dev js) ; Jacob Groß - interesting post about "Breaking up with SVG-in-JS"
          ("https://joshcollinsworth.com/api/rss.xml" dev js css) ; Josh Collinsworth
          ("https://www.joshwcomeau.com/rss.xml" dev js css) ; Josh Comeau - got some great interactive posts on CSS Grid and Flexbox
          ("https://nolanlawson.com/feed/" dev js) ; Nolan Lawson's blog
          ("https://dassur.ma/index.xml" dev js)   ; Surma
          ("http://feeds.feedburner.com/tommacwright/odWX" dev js) ; Tom MacWright
          ("https://devblogs.microsoft.com/typescript/feed/" dev js) ; Typescript releases

          ;; Dev - kotlin/java
          ("http://blog.jetbrains.com/kotlin/feed/" dev kotlin)

          ;; Emacs
          ("https://karthinks.com/index.xml" emacs) ; Karthinks blog on Emacs (author of gptel and lots of other packages)
          ("https://peiwen.lu/rss.xml" emacs) ; Has the jtsx package, which is promising, but slightly conflicting with Combobulate / lsp

          ;; Opionions and news
          ("https://world.hey.com/dhh/feed.atom" opinions) ; DHH's (increasingly controversial) opinions
          ("https://daringfireball.net/feeds/json" opinions) ; News and opinions from John Gruber, the author of Markdown
          ("https://news.ycombinator.com/rss" news) ; Hacker news

          ;; Music
          ("http://motorpsycho.no/feed/" music)

          ;; Comics
          ("https://xkcd.com/atom.xml" comics))))

(use-package elfeed-org
  :ensure t
  :custom
  (rmh-elfeed-org-files (list "~/org/elfeed.org"))
  :config
  (elfeed-org))

(provide 'setup-elfeed)
