all : linux.html mac.html windows.html contents.html

%.html : %.markdown
	pandoc --from=markdown --to=html \
		--standalone \
		--css=style-info.css \
	    --include-after-body analytics.js \
		--output=$@ $<
