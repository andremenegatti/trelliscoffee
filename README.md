# Why this repository exists?
During my Master's research on automated bidding in public procurement auctions, I found myself manually plotting data regarding dozens of coffee auctions in order to inspect bidding behaviour. This was time-consuming, error-prone and outright boring. And since my database contains thousands of auctions, this approach would simply not allow me to see the broader picture.

Enters `trelliscopejs`. This phenomenal R package by [Ryan Hafen](https://github.com/hafen) allows you to easily build an interactive web page with a catalogue of plots, which can then be filtered, sorted and labeled in many useful ways. With `trelliscopejs`, instead of exploring my dataset to find potentially interesting auctions and _then_ plotting them, the plots for any set of auctions are just a few clicks/keystrokes away. Exploring auction metadata and visualizing bids are no longer two separate steps.

# What is inside?
In this repo, you can find:
- **cnet_lances.rds**. The database I built with the bids submitted in all coffee procurement auctions hosted in the Comprasnet platform (an online procurement platform used by entities of the Brazilian Federal Government) from March/2011 to December/2016.
- **auction_plotly.R**. R function that builds a two-pannel interactive plot for a single auction. The top pannel is a scatterplot of bid value (R$/kg of coffee) vs. time. The bottom pannel is a scatterplot of bid increment vs. time, _i.e._, it shows how the differences between bids evolve during the auction. Both pannels only include bids submitted during the final (or 'random') phase of a given auction (which is when most of the action is).
- **cnet_trelliscope.R**: R script that uses `trelliscopejs` to build the catalogue of interactive plots for every auction in the database.
- **index.html**. The interactive webpage generated by **cnet_trelliscope.R**. After cloning the repo, you can simply open this file using your favorite browser to see how cool `trelliscopejs` is. Folders **appfiles** and **lib** contain the files created by `trelliscopejs` that make all the magic work.