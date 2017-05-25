ATBinHex was originally a Delphi component http://atviewer.sourceforge.net/atbinhex.htm

This is Lazarus version.

* Some features disabled via ATBinHexOptions.inc (searching of text, printing, regex hiliting of URLs).
* Removed method OpenFile, it used Win API, now use OpenStream.
* Encodings are supported again, via LConvEncoding, in non-unicode modes only
