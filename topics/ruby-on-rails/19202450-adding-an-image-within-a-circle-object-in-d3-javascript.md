
# Adding an image within a circle object in d3 javascript?

## Question
        
My goal is to add an image into an existing circle with d3. The circle will render and is interactive with mouseover method, but only when I use "fill", "color", and not something more sophisticated like .append("image").

          g.append("circle")
             .attr("class", "logo")
             .attr("cx", 700)
             .attr("cy", 300)
             .attr("r", 10)
             .attr("fill", "black")       // this code works OK
             .attr("stroke", "white")     // displays small black dot
             .attr("stroke-width", 0.25)
             .on("mouseover", function(){ // when I use .style("fill", "red") here, it works 
                   d3.select(this)        
                       .append("svg:image")
                       .attr("xlink:href", "/assets/images/logo.jpeg")
                       .attr("cx", 700)
                       .attr("cy", 300)
                       .attr("height", 10)
                       .attr("width", 10);
             });
    

The image doesn't show after I mouse over. Using Ruby on Rails app, where my image "logo.jpeg" is stored in the assets/images/ directory. Any help for getting my logo to show within the circle? Thanks.

## Answer
        
As Lars says you need to use pattern, once you do that it becomes pretty straightforward. Here's a link to a [conversation](https://groups.google.com/forum/#!msg/d3-js/Cv08x3eCxVE/N1MkjD8fMTEJ) in d3 google groups about this. I've set up a [fiddle](http://jsfiddle.net/7DgUh/) here using the image of a pint from that conversation and your code above.

To set up the pattern:

        <svg id="mySvg" width="80" height="80">
          <defs id="mdef">
            <pattern id="image" x="0" y="0" height="40" width="40">
              <image x="0" y="0" width="40" height="40" xlink:href="http://www.e-pint.com/epint.jpg"></image>
            </pattern>
      </defs>
    

Then the d3 where we only change the fill:

    svg.append("circle")
             .attr("class", "logo")
             .attr("cx", 225)
             .attr("cy", 225)
             .attr("r", 20)
             .style("fill", "transparent")       
             .style("stroke", "black")     
             .style("stroke-width", 0.25)
             .on("mouseover", function(){ 
                   d3.select(this)
                       .style("fill", "url(#image)");
             })
              .on("mouseout", function(){ 
                   d3.select(this)
                       .style("fill", "transparent");
             });
