#' Vectors of colors for my plots
#'
#' Copyright (C) 2018 Karl Broman
#' GNU General Public License, version 3, available at https://www.r-project.org/Licenses/GPL-3
#'
#' This function Creates different vectors of colors that I use for plots. It
#' was inspired by Karl Broman's brocolors.R function:
#' https://github.com/kbroman/pkg_primer/blob/gh-pages/example/stage5/R/brocolors.R
#' which he uses in his minimal R package example. I have copied a number of
#' pieces of that, particularly the crayon colors specifications.
#'
#' @param set Character string indicating a set of colors.
#'
#' @return Vector of character strings representing the chosen set of colors, in RGB.
#'
#' @author Richard T Gray, \email{Rgray@kirby.unsw.edu.au}
#' @references \url{https://github.com/kbroman/pkg_primer/blob/gh-pages/example/stage5/R/brocolors.R}
#' @references \url{http://en.wikipedia.org/wiki/List_of_Crayola_crayon_colors}
#' @seealso \code{\link{PlotCrayons}}
# @seealso \code{\link{LineOptions}}
#'
#' @examples
#' PlotColors()
#'
#' @export
#'
PlotColors <- function(set = c("main", "lines", "cbPalette", "hivcascade4",
  "hivcascade5", "crayons", "dracula")) {

  set <- match.arg(set)

  # Generate all the colour vector options -------------------------------------

  # Palette with black and grey (colour blind friendly)
  cbPalette <- c("#000000","#999999", "#E69F00", "#56B4E9", "#009E73",
    "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

  # Colour scheme used in ASR
  hivcascade4 <- c("#621C20", "#AB2322", "#D95728", "#E97164")
  hivcascade5 <- c("#621C20", "#AB2322", "#A63603", "#D95728", "#E97164")
  #sticasade <-

  # Setup crayon colours which are used for most of my main plots
  crayons <- c("Almond"="#efdecd",
    "Antique Brass"="#cd9575",
    "Apricot"="#fdd9b5",
    "Aquamarine"="#78dbe2",
    "Asparagus"="#87a96b",
    "Atomic Tangerine"="#ffa474",
    "Banana Mania"="#fae7b5",
    "Beaver"="#9f8170",
    "Bittersweet"="#fd7c6e",
    "Black"="#000000",
    "Blizzard Blue"="#ace5ee",
    "Blue"="#1f75fe",
    "Blue Bell"="#a2a2d0",
    "Blue Gray"="#6699cc",
    "Blue Green"="#0d98ba",
    "Blue Violet"="#7366bd",
    "Blush"="#de5d83",
    "Brick Red"="#cb4154",
    "Brown"="#b4674d",
    "Burnt Orange"="#ff7f49",
    "Burnt Sienna"="#ea7e5d",
    "Cadet Blue"="#b0b7c6",
    "Canary"="#ffff99",
    "Caribbean Green"="#00CC99",
    "Carnation Pink"="#ffaacc",
    "Cerise"="#dd4492",
    "Cerulean"="#1dacd6",
    "Chestnut"="#bc5d58",
    "Copper"="#dd9475",
    "Cornflower"="#9aceeb",
    "Cotton Candy"="#ffbcd9",
    "Dandelion"="#fddb6d",
    "Denim"="#2b6cc4",
    "Desert Sand"="#efcdb8",
    "Eggplant"="#6e5160",
    "Electric Lime"="#ceff1d",
    "Fern"="#71bc78",
    "Forest Green"="#6dae81",
    "Fuchsia"="#c364c5",
    "Fuzzy Wuzzy"="#cc6666",
    "Gold"="#e7c697",
    "Goldenrod"="#fcd975",
    "Granny Smith Apple"="#a8e4a0",
    "Gray"="#95918c",
    "Green"="#1cac78",
    "Green Blue"="#1164b4",
    "Green Yellow"="#f0e891",
    "Hot Magenta"="#ff1dce",
    "Inchworm"="#b2ec5d",
    "Indigo"="#5d76cb",
    "Jazzberry Jam"="#ca3767",
    "Jungle Green"="#3bb08f",
    "Laser Lemon"="#fefe22",
    "Lavender"="#fcb4d5",
    "Lemon Yellow"="#fff44f",
    "Macaroni and Cheese"="#ffbd88",
    "Magenta"="#f664af",
    "Magic Mint"="#aaf0d1",
    "Mahogany"="#cd4a4c",
    "Maize"="#edd19c",
    "Manatee"="#979aaa",
    "Mango Tango"="#ff8243",
    "Maroon"="#c8385a",
    "Mauvelous"="#ef98aa",
    "Melon"="#fdbcb4",
    "Midnight Blue"="#1a4876",
    "Mountain Meadow"="#30ba8f",
    "Mulberry"="#c54b8c",
    "Navy Blue"="#1974d2",
    "Neon Carrot"="#ffa343",
    "Olive Green"="#bab86c",
    "Orange"="#ff7538",
    "Orange Red"="#ff2b2b",
    "Orange Yellow"="#f8d568",
    "Orchid"="#e6a8d7",
    "Outer Space"="#414a4c",
    "Outrageous Orange"="#ff6e4a",
    "Pacific Blue"="#1ca9c9",
    "Peach"="#ffcfab",
    "Periwinkle"="#c5d0e6",
    "Piggy Pink"="#fddde6",
    "Pine Green"="#158078",
    "Pink Flamingo"="#fc74fd",
    "Pink Sherbert"="#f78fa7",
    "Plum"="#8e4585",
    "Purple Heart"="#7442c8",
    "Purple Mountain's Majesty"="#9d81ba",
    "Purple Pizzazz"="#fe4eda",
    "Radical Red"="#ff496c",
    "Raw Sienna"="#d68a59",
    "Raw Umber"="#714b23",
    "Razzle Dazzle Rose"="#ff48d0",
    "Razzmatazz"="#e3256b",
    "Red"="#ee204d",
    "Red Orange"="#ff5349",
    "Red Violet"="#c0448f",
    "Robin's Egg Blue"="#1fcecb",
    "Royal Purple"="#7851a9",
    "Salmon"="#ff9baa",
    "Scarlet"="#fc2847",
    "Screamin' Green"="#76ff7a",
    "Sea Green"="#93dfb8",
    "Sepia"="#a5694f",
    "Shadow"="#8a795d",
    "Shamrock"="#45cea2",
    "Shocking Pink"="#fb7efd",
    "Silver"="#cdc5c2",
    "Sky Blue"="#80daeb",
    "Spring Green"="#eceabe",
    "Sunglow"="#ffcf48",
    "Sunset Orange"="#fd5e53",
    "Tan"="#faa76c",
    "Teal Blue"="#18a7b5",
    "Thistle"="#ebc7df",
    "Tickle Me Pink"="#fc89ac",
    "Timberwolf"="#dbd7d2",
    "Tropical Rain Forest"="#17806d",
    "Tumbleweed"="#deaa88",
    "Turquoise Blue"="#77dde7",
    "Unmellow Yellow"="#ffff66",
    "Violet (Purple)"="#926eae",
    "Violet Blue"="#324ab2",
    "Violet Red"="#f75394",
    "Vivid Tangerine"="#ffa089",
    "Vivid Violet"="#8f509d",
    "White"="#FFFFFF",
    "Wild Blue Yonder"="#a2add0",
    "Wild Strawberry"="#ff43a4",
    "Wild Watermelon"="#fc6c85",
    "Wisteria"="#cda4de",
    "Yellow"="#fce883",
    "Yellow Green"="#c5e384",
    "Yellow Orange"="#ffae42")

  # Setup dracula theme colours
  # https://draculatheme.com/
  # https://en.wikipedia.org/wiki/Dracula_(color_scheme)
  dracula <- c("Background"="#282a36",
    "Current Line"="#44475a",
    "Foreground"="#f8f8f2",
    "Comment"="#6272a4",
    "Cyan"="#8be9fd",
    "Green"="#50fa7b",
    "Orange"="#ffb86c",
    "Pink"="#ff79c6",
    "Purple"="#bd93f9",
    "Red"="#ff5555",
    "Yellow"="#f1fa8c")
  
  
  # Main/general colours I use --------------------------------------------

  # The 10 main colours I like using
  main <- crayons[c("Black", "Blue", "Red", "Green", "Orange",
    "Violet (Purple)", "Cerulean", "Raw Umber", "Pine Green", 
    "Hot Magenta")]

  # Five main colours for line plots. I don't think you should have more 
  # than 5 lines in a line plot. Preferably less.
  lines <- crayons[c("Black", "Blue", "Red", "Green", "Orange")]

  # Generate and return final set of colours ------------------------------
  colours <- switch(match.arg(set),
    main=main,
    lines=lines,
    cbPalette=cbPalette,
    hivcascade4=hivcascade4,
    hivcascade5=hivcascade5,
    crayons=crayons,
    dracula=dracula)

  # Return final vector
  return(colours)

}
