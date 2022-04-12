// * define shuffle functions *

// step 1: randomize array
// source: https://stackoverflow.com/questions/2450954/how-to-randomize-shuffle-a-javascript-array

function randomize(array) {
    var currentIndex = array.length, temporaryValue, randomIndex;
  
    // While there remain elements to shuffle...
    while (0 !== currentIndex) {
  
      // Pick a remaining element...
      randomIndex = Math.floor(Math.random() * currentIndex);
      currentIndex -= 1;
  
      // And swap it with the current element.
      temporaryValue = array[currentIndex];
      array[currentIndex] = array[randomIndex];
      array[randomIndex] = temporaryValue;
    }
  
    return array;
  }

// step 2: shuffle two arrays
// source: https://github.com/addrummond/ibex/blob/master/www/shuffle.js

// shuffle an array.
function fisherYates(myArray) {
    var i = myArray.length;
    if (i == 0)
        return false;
    while (--i) {
        var j = Math.floor(Math.random() * (i + 1));
        var tempi = myArray[i];
        var tempj = myArray[j];
        myArray[i] = tempj;
        myArray[j] = tempi;
   }
}

// given an array of arrays, returns a single array with elements from each array shuffled evenly into it. WARNING: This will destructively modify the matrix array. IMPORTANT: The order of elements in subarrays is preserved.

function shuffle(arrayOfArrays) {
    fisherYates(arrayOfArrays);

    var longestArrayLength = 0;
    var totalLength = 0;
    for (var i = 0; i < arrayOfArrays.length; ++i) {
        if (arrayOfArrays[i].length > longestArrayLength)
            longestArrayLength = arrayOfArrays[i].length;
        totalLength += arrayOfArrays[i].length;
    }

    if (totalLength == 0) { return []; }

    var loopsPerIncrement = new Array(arrayOfArrays.length);
    for (var i = 0; i < arrayOfArrays.length; ++i) {
        loopsPerIncrement[i] = (arrayOfArrays[i].length + 0.0) / (longestArrayLength + 0.0)
    }

    var indexArray = new Array(arrayOfArrays.length);
    for (var i = 0; i < indexArray.length; ++i) {
        indexArray[i] = 0.0;
    }

    var shuffledArray = new Array(totalLength);
    for (var idx = 0; idx < totalLength;) {
        for (var j = 0; j < arrayOfArrays.length; ++j) {
            var oldi = indexArray[j];
            var newi = oldi + loopsPerIncrement[j];
            if (Math.floor(oldi) != Math.floor(newi)) {
                if (oldi < arrayOfArrays[j].length) {
                    shuffledArray[idx] = arrayOfArrays[j][Math.floor(oldi)];
                    ++idx;
                    if (! (idx < totalLength)) {
                        break; // The outer loop will now exit too.
                    }
                }
            }
            indexArray[j] = newi;
        }
    }

    return shuffledArray;
}

// step 3: combine the two functions to efficiently randomize and shuffle multiple arrays

function rshuffle() {
    var arrayOfArrays = [];
    for (var i = 0; i < arguments.length; i++) {
        var randomized = randomize(arguments[i]);
        arrayOfArrays.push(randomized);
    }
    shuffled = shuffle(arrayOfArrays);
    return shuffled;
  };