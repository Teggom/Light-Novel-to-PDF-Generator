# Light-Novel-to-PDF-Generator

# Requires LaTex 

###! These might not work anymore if something has changed. 

There are websites out there that host fan-translated material that has not been localized to the United States or into English yet. 
Two of these websites are BakaTsuki and NovelPlanet. 

These websites tend to host the translated text on the HTML page directly. I wanted to write a program that gathered all of these and converted it into a PDF file. 

First came BakaTsuki. This implementation focused a need on specifying which series you wanted to gather. It then goes to each page, gathers the data, cleans it up, and then formats it in LaTex to a PDF. 

Second came NovelPlanet. For this I took the idea I had with Baka Tsuki and generalized it, as well as sped it up. 
This program will find almost every novel hosted on NovelPlanet, and gather that series. 
	It does this by running though and gathering all of the URLs for each novel first, then going through and actually gathering the data, then converting the data into a PDF file. 
	Should a series not have specific chapters already preset, as is the case with webnovels, it will compile several of them together into a 300-400 page book. 

Note: This program could stand to be cleaned up and made faster, as it is quite slow. My implementation of regex could be sped up and dramatically increase the current 4+ hour runtime to much much less. 

This program generates around 11,000 Books!

	