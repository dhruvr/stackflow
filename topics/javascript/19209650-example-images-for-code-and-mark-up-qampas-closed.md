
# Example images for code and mark-up Q&amp;As [closed]

## Question
        
When preparing an [MCVE](https://stackoverflow.com/help/mcve)/[SSCCE](http://sscce.org/) that involves images, it is useful to have direct access to images.

The types of images that would cover most questions are - small images in multiple colors or shapes, animated GIFs with/without transparency, JPEGs that are 'pairs' of pictures & can be used in image transitions, tile sets, sprite sheets..

Are there any small (under 30KB), on-site, license & royalty free images we can hot-link to for these types of examples?

## Answer
        
Here are some example images for common use, mostly from existing answers on SO.

**Icons**
=========

**Simple Geometric shapes** generated using Java as originally seen in [this answer](https://stackoverflow.com/a/16052085/418556). It includes a Java based interface that defines the URLs and makes them easy to access.

**Details:** 32x32 pixel PNG (4 colors x 5 shapes) with partial transparency (along the edges).

**Categories:** [png](/questions/tagged/png "show questions tagged 'png'") [icons](/questions/tagged/icons "show questions tagged 'icons'")

> ![](https://i.stack.imgur.com/gJmeJ.png) ![](https://i.stack.imgur.com/L5DGx.png) ![](https://i.stack.imgur.com/in9g1.png) ![](https://i.stack.imgur.com/IucNt.png) ![](https://i.stack.imgur.com/yoKxT.png)   ![](https://i.stack.imgur.com/T5uTa.png) ![](https://i.stack.imgur.com/gYxHm.png) ![](https://i.stack.imgur.com/1lgtq.png) ![](https://i.stack.imgur.com/yBOv3.png) ![](https://i.stack.imgur.com/zJ8am.png)  
> ![](https://i.stack.imgur.com/IHARa.png) ![](https://i.stack.imgur.com/8BGfi.png) ![](https://i.stack.imgur.com/6ZXhi.png) ![](https://i.stack.imgur.com/Lqkl0.png) ![](https://i.stack.imgur.com/c67nr.png)   ![](https://i.stack.imgur.com/wCF8S.png) ![](https://i.stack.imgur.com/5v2TX.png) ![](https://i.stack.imgur.com/F0JHK.png) ![](https://i.stack.imgur.com/4EVv1.png) ![](https://i.stack.imgur.com/xj49g.png)  

Sprite Sheets
-------------

**Chess Pieces** as originally seen on [this answer](https://stackoverflow.com/a/18686753/418556) that includes 2 other sprite sets (same image in different colors).

**Details:** 384x128 px (each sprite 64x64 px) PNG with partial transparency.

**Categories:** [png](/questions/tagged/png "show questions tagged 'png'") [sprite-sheet](/questions/tagged/sprite-sheet "show questions tagged 'sprite-sheet'") [icons](/questions/tagged/icons "show questions tagged 'icons'")

> ![](https://i.stack.imgur.com/memI0.png)

Animated
--------

GIF is the only image format that supports animation. Here are a few examples.

**Categories:** [gif](/questions/tagged/gif "show questions tagged 'gif'") [animated-gif](/questions/tagged/animated-gif "show questions tagged 'animated-gif'")

### Solid BG

**Animated dashed border** as seen in [this answer](https://stackoverflow.com/a/9772978/418556).

**Details:** 100x30 px with filled BG (no transparency)

> ![](https://i.stack.imgur.com/DnzXV.gif)

**Zooming stars** as seen in [this answer](https://stackoverflow.com/a/10836833/418556), originally developed as a 'screen-shot' of a screensaver.

**Details:** 160x120 px with filled BG (no transparency)

> ![](https://i.stack.imgur.com/iQFxo.gif)

**Animated Water** as seen in [this answer](https://stackoverflow.com/a/17894012/418556) to [Animating Tiles In My Game](https://stackoverflow.com/q/17893429/418556).

**Details:** 60x60 px with filled BG (no transparency)

> ![](https://i.stack.imgur.com/HXCUV.gif)

### Transparent BG

**Orbital animation,** originally developed for [1.1C](http://1point1c.org/). The orbits of the 'inner' planets (from Mercury to Jupiter, with an extra orbit shown in the thick of the asteroid belt). Better on a dark BG.

**Details:** 450x450 & 150x150 px animated GIFs with transparency.

> ![](https://i.stack.imgur.com/OtTIY.gif)![](https://i.stack.imgur.com/z9rv7.gif)

Pictures
--------

**Sunrise & moonset** over the CBD of Sydney, Australia  
**Sunset & Venus over a telescope** on Mt Stromlo, near Canberra, Australia.

**Categories:** [jpeg](/questions/tagged/jpeg "show questions tagged 'jpeg'") [slideshow](/questions/tagged/slideshow "show questions tagged 'slideshow'") \+ Image Transitions

**Details:** 480x320 px JPEGs x 4. (Displayed here at 1/2 size.)

> ![](https://i.stack.imgur.com/XZ4V5.jpg)![](https://i.stack.imgur.com/7bI1Y.jpg)  
> ![](https://i.stack.imgur.com/OVOg3.jpg)![](https://i.stack.imgur.com/lxthA.jpg)

**Panorama at Dawn** across the South-Eastern Suburbs of Sydney.

**Categories:** [jpg](/questions/tagged/jpg "show questions tagged 'jpg'") [panoramas](/questions/tagged/panoramas "show questions tagged 'panoramas'") [animation](/questions/tagged/animation "show questions tagged 'animation'") (scrolling)

**Details:** 1474x436 px JPEG.

> [![Dawn Panorama](https://i.stack.imgur.com/aH5zB.jpg)](https://i.stack.imgur.com/aH5zB.jpg)

Tiles
-----

This **Mercator map of Earth** can be tiled left/right. Originally seen on [this answer](https://stackoverflow.com/a/18825844/418556). The answer also includes a second version of the image that shows a semi-transparent line for the equator (which is not in the center, but significantly below it).

**Details:** 640x316 px (add 44 px at bottom to center equator) PNG with transparent BG.

**Categories:** [png](/questions/tagged/png "show questions tagged 'png'") [tile](/questions/tagged/tile "show questions tagged 'tile'") [animation](/questions/tagged/animation "show questions tagged 'animation'") (scrolling)

> ![](https://i.stack.imgur.com/P59NF.png)

Tip
---

For getting the URLs of the images, you might 'context click' on the image as seen in the browser and either:

*   Show the properties. The URL can be copied from the dialog that appears.
*   View image. Copy the URL from the browser address bar.

Alternately:

*   Use the browser 'show source' and copy it from the HTML.
*   For those with enough rep. (100+, to edit a community Wiki answer), go to [edit the answer](https://stackoverflow.com/posts/19209651/edit) and pull the URL from the text.

Code
----

Below is a Java class which splits up the chess piece sprite sheet, suitable for pasting in to an MCVE:

    import java.awt.image.*;
    import javax.imageio.*;
    import java.net.*;
    import java.io.*;
    import java.util.*;
    
    public final class ChessSprites {
        private ChessSprites() {}
        public static final int SIZE = 64;
        public static final BufferedImage SHEET;
        static {
            try {
                // see https://stackoverflow.com/a/19209651/2891664
                SHEET = ImageIO.read(new URL("https://i.stack.imgur.com/memI0.png"));
            } catch (IOException x) {
                throw new UncheckedIOException(x);
            }
        }
        public static final BufferedImage GOLD_QUEEN    = SHEET.getSubimage(0 * SIZE, 0,    SIZE, SIZE);
        public static final BufferedImage SILVER_QUEEN  = SHEET.getSubimage(0 * SIZE, SIZE, SIZE, SIZE);
        public static final BufferedImage GOLD_KING     = SHEET.getSubimage(1 * SIZE, 0,    SIZE, SIZE);
        public static final BufferedImage SILVER_KING   = SHEET.getSubimage(1 * SIZE, SIZE, SIZE, SIZE);
        public static final BufferedImage GOLD_ROOK     = SHEET.getSubimage(2 * SIZE, 0,    SIZE, SIZE);
        public static final BufferedImage SILVER_ROOK   = SHEET.getSubimage(2 * SIZE, SIZE, SIZE, SIZE);
        public static final BufferedImage GOLD_KNIGHT   = SHEET.getSubimage(3 * SIZE, 0,    SIZE, SIZE);
        public static final BufferedImage SILVER_KNIGHT = SHEET.getSubimage(3 * SIZE, SIZE, SIZE, SIZE);
        public static final BufferedImage GOLD_BISHOP   = SHEET.getSubimage(4 * SIZE, 0,    SIZE, SIZE);
        public static final BufferedImage SILVER_BISHOP = SHEET.getSubimage(4 * SIZE, SIZE, SIZE, SIZE);
        public static final BufferedImage GOLD_PAWN     = SHEET.getSubimage(5 * SIZE, 0,    SIZE, SIZE);
        public static final BufferedImage SILVER_PAWN   = SHEET.getSubimage(5 * SIZE, SIZE, SIZE, SIZE);
        public static final List<BufferedImage> SPRITES =
            Collections.unmodifiableList(Arrays.asList(GOLD_QUEEN,  SILVER_QUEEN,
                                                       GOLD_KING,   SILVER_KING,
                                                       GOLD_ROOK,   SILVER_ROOK,
                                                       GOLD_KNIGHT, SILVER_KNIGHT,
                                                       GOLD_BISHOP, SILVER_BISHOP,
                                                       GOLD_PAWN,   SILVER_PAWN));
    }
