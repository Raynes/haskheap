# Haskheap

This is a simple Haskell library for easily interacting with the refheap API.

# Usage

You'll want to `import Network.Haskheap` first of all.

```haskell
*Network.Haskheap> createPaste "foo" True "Haskell" (Just ("raynes","35155a3f-4db3-4495-9934-7a24b9c4eed8"))

Right (Paste {getLines = 1, getDate = Just 2012-11-28 09:36:41.399 UTC, getID = "c927b8cd91945606a97907182", getLanguage = "Haskell", getPrivate = True, getURL = Just https://www.refheap.com/paste/c927b8cd91945606a97907182, getUser = Just "raynes", getBody = "foo"})


*Network.Haskheap> getPaste "c927b8cd91945606a97907182"

Right (Paste {getLines = 1, getDate = Just 2012-11-28 09:36:41.399 UTC, getID = "c927b8cd91945606a97907182", getLanguage = "Haskell", getPrivate = True, getURL = Just https://www.refheap.com/paste/c927b8cd91945606a97907182, getUser = Just "raynes", getBody = "foo"})


*Network.Haskheap> editPaste "c927b8cd91945606a97907182" "foo2" True "Haskell" ("raynes","35155a3f-4db3-4495-9934-7a24b9c4eed8")

Right (Paste {getLines = 1, getDate = Just 2012-11-28 09:36:41.399 UTC, getID = "c927b8cd91945606a97907182", getLanguage = "Haskell", getPrivate = True, getURL = Just https://www.refheap.com/paste/c927b8cd91945606a97907182, getUser = Just "raynes", getBody = "foo2"})


*Network.Haskheap> getHighlightedPaste "c927b8cd91945606a97907182"

Right (Line "<table class=\"highlighttable\"><tr><td class=\"linenos\"><div class=\"linenodiv\"><pre><a href=\"#L-1\">1</a></pre></div></td><td class=\"code\"><div class=\"highlight\"><pre><a name=\"L-1\"></a><span class=\"nf\">foo2</span>\n</pre></div>\n</td></tr></table>")


*Network.Haskheap> deletePaste "c927b8cd91945606a97907182" ("raynes","35155a3f-4db3-4495-9934-7a24b9c4eed8")

Right Empty


*Network.Haskheap> forkPaste "7097" ("raynes","35155a3f-4db3-4495-9934-7a24b9c4eed8")

Right (Paste {getLines = 25, getDate = Just 2012-11-28 09:41:12.177 UTC, getID = "7104", getLanguage = "Clojure", getPrivate = False, getURL = Just https://www.refheap.com/paste/7104, getUser = Just "raynes", getBody = "(ns pimp.client\r\n  (:require [clojure.java.io :as io]\r\n            [clojure.xml :as xml])\r\n  (:import [java.net URLEncoder]))\r\n\r\n(def ^:dynamic *artist-base-url*\r\n  \"http://www.musicbrainz.org/ws/2/artist/\")\r\n\r\n(defn artist-url\r\n  [artist] (str *artist-base-url* \"?query=artist:\" (URLEncoder/encode artist)))\r\n\r\n(def tag-content\r\n  (comp :content first filter))\r\n\r\n(defn artist-tags\r\n  [artist]\r\n  (->>  artist artist-url io/input-stream xml/parse xml-seq\r\n        (tag-content #(and (= :artist (:tag %))\r\n                           (= \"100\" (get-in % [:attrs :ext:score]))))\r\n        (tag-content #(= :tag-list (:tag %)))\r\n        (mapcat :content) (map #(get-in % [:content 0]))))\r\n\r\n(artist-tags \"The Decemberists\")\r\n;; (\"rock\" \"american\" \"indie rock\" \"baroque pop\" \"indie pop\"\r\n;;  \"progressive folk\" \"classic pop and rock\")\r\n"})
```

