
let observer;

//write to clipboard
export async function writeToClipboard(text) {
  await navigator.clipboard.writeText(text);
}

export async function readTextFromClipboard() {
  return await navigator.clipboard.readText();
}

// Function to get the current window width
export function getWindowWidth() {
  return window.innerWidth;
}

export function getWindowHeight() {
  return window.innerHeight;
}

export function getFullScreenSize() {
  return {
    width: window.screen.width,
    height: window.screen.height
  };
}

// returns the *viewport* size (excludes browser chrome, OS taskbars, etc.)
export function getViewportSize() {
    return {
        width: window.innerWidth,
        height: window.innerHeight
    };
}

export function getUnzoomedViewportHeight() {
    // visualViewport gives you both height (in CSS px) and the current zoom scale
    const zoom = window.screen.width / window.innerWidth;
    return window.innerHeight * zoom;    
}

console.log("Height at 100% zoom:", getUnzoomedViewportHeight());


//function to calculate the height of all elements with the given class name
export function calculateHeightByClassName(className) {
  var elements = document.getElementsByClassName(className);
  var height = 0;
  for (var i = 0; i < elements.length; i++) {
    height += elements[i].offsetHeight;
  }
  return height;
}

//function to calculate the height of all elements under a given div element
export function calculateHeightByElementId(id) {
  var element = document.getElementById(id);
  var height = 0;
  if (element) {
    var children = element.children;
    for (var i = 0; i < children.length; i++) {
      height += children[i].offsetHeight;
    }
  }
  return height;
}

export function registerResizeEvent(dotnetReference) {
  window.addEventListener("resize", () => {
    dotnetReference.invokeMethodAsync('OnBrowserResize');
  });
}

export function unregisterResizeCallback() {
  window.removeEventListener("resize", () => {
    // Handle removal if needed
  });
}

// This function triggers the resize event on the window.
export function triggerResizeEvent() {
  //console.log('triggered an resize event');
  window.dispatchEvent(new Event('resize'));
}

export function updateCarouselHeight() {
  var carousel = document.querySelector('.myMudCarousel');
  var items = carousel.querySelectorAll('.mud-carousel-item');
  var maxHeight = 0;

  // Calculate the max height of the carousel items
  items.forEach(function (item) {
    var itemHeight = item.getBoundingClientRect().height;
    if (itemHeight > maxHeight) {
      maxHeight = item.offsetHeight;
    }
  });

  // Apply the max height to the carousel
  carousel.style.height = maxHeight + 'px';
}

export function openNewWindowAndWriteContent(content) {
  var newWindow = window.open("", "_blank");
  newWindow.document.write('<pre>' + content + '</pre>');
  newWindow.document.close();
}

export function setWidthAndHeight(board) {
  const resizeHandler = () => board.resize();
  window.addEventListener('resize', resizeHandler);

  // Return an object with a dispose method for cleanup
  return {
    dispose: () => window.removeEventListener('resize', resizeHandler)
  };
}

export function chessBoardToElementWithResizing(boardId, fen) {  
  const element = document.getElementById(boardId);
  createChessboard(element, fen);
}

export function resizeChessboard(board) {
  // Your resize logic here
  if (board) {
    board.resize();
  }
}

function clearHighlightSquaresForElement(element) {
  if (element && element.board) {
    element.querySelectorAll('.highlight-white').forEach(square => square.classList.remove('highlight-white'));
    element.querySelectorAll('.highlight-black').forEach(square => square.classList.remove('highlight-black'));
  }
}

// Private function to ensure that a chessboard instance exists on the element
function ensureChessboard(element, config) {

  if (!element.board) {
    var board = Chessboard2(element, config);
    element.board = board;
    element.arrow2Id = null;
    element.ponderArrowId = null;
    element.circleId = null;
    element.circleId2 = null;
    element.board.resize();
  }
}

export function createChessboard(element, fen) {

  if (!element) {
    console.log('element is null');
  }
  else {
    //check for undefined board
    if (!element.board) {
      var board = Chessboard2(element, 'start');
      if (fen) {
        board.position(fen, false);
      }
      element.board = board;
      element.arrow2Id = null;
      element.ponderArrowId = null;
      element.circleId = null;
      element.circleId2 = null;
    }
    else {
      if (fen) {
        resizeChessboard2(element);
      }
      else {
        clearHighlightSquaresForElement(element);
        element.board.position(fen, false);
        resizeChessboard2(element);
      }
    }
  }
}

// resize the chessboard
export function resizeChessboard2(element) {
  // Your resize logic here
  if (element.board) {
    element.board.resize();
  }
  else { console.log('element.board is null'); }
}

//resizing pv boards
export function adjustElement(element, size) {
  // Your resize logic here
  if (element.board) {
    //if element size is different from size, set the size to the element    
    element.style.width = size + 'px';
    element.style.height = size + 'px';
    element.board.resize();
    // }

    element.style.border = '4px solid black';
  }
  else {
    console.log('element.board is null');
  }
}

//function to adjust all pv boards
export function adjustAllPVboards(size) {
  let allBoards = document.getElementsByClassName('pvBoard');
  //loop through all boards and adjust the size
  for (let board of allBoards) {
    adjustElement(board, size);
  }
}

// Exported function to set the position of the chessboard
export function setPosition2(element, fen) {
  var config = {
    showNotation: false,
    position: fen
  };

  ensureChessboard(element, config);
  let board = element.board;

  if (element.arrow2Id) {
    board.removeArrow(element.arrow2Id);
  }
  clearHighlightSquaresForElement(element);
  board.position(fen, false);
}

// Exported function to clear the chessboard
export function clearBoard2(element) {
  var config = {
    showNotation: false,
    position: 'start',
    useAnimation: false
  };
  ensureChessboard(element, config);
  element.board.clear();
  clearHighlightSquaresForElement(element);
}

export function highlightSquare2(element, squareCoord, color, remove) {
  var highlightClass = color === 'w' ? 'highlight-white' : 'highlight-black';
  if (remove) {
    // Remove any existing highlights
    clearHighlightSquaresForElement(element);
  }

  // Select the square element using its data-square-coord attribute
  const square = element.querySelector(`[data-square-coord="${squareCoord}"]`);

  // Check if the square exists
  if (square) {
    // Add the highlight class to the square
    square.classList.add(highlightClass);
  } else {
    console.error(`Square with coord ${squareCoord} not found.`);
  }
}

// Function to remove all existing circle pieces
function removeCirclePieces() {
  var circlePieces = document.getElementsByClassName('circle-piece');
  for (let circlePiece of circlePieces) {
    circlePiece.remove();
  }
}

export function setLabel(element, sq, text, textColor, bgColor) {
  let board = element.board
  //add fromSq to toSq arrow
  if (board && sq && text) {
    removeCirclePieces();
    // Create a new piece with a label of toSq
    var piece = document.createElement('div');
    piece.className = 'circle-piece'; //maybe better name is : circle-annotation
    piece.textContent = text;
    const square = element.querySelector(`[data-square-coord="${sq}"]`);
    piece.style.color = textColor;
    piece.style.backgroundColor = bgColor;
    square.appendChild(piece);
  }
}

export function setCircleHighlighting(element, fen, fromSq, toSq, color) {

  let board = element.board
  //add fromSq to toSq arrow
  if (board && fromSq && toSq) {
    if (element.arrow2Id) {
      board.removeArrow(element.arrow2Id);
    }
    if (element.circleId) {
      board.removeCircle(element.circleId);
    }
    if (element.circleId2) {
      board.removeCircle(element.circleId2);
    }

    removeCirclePieces();

    element.board.position(fen, false);

    element.circleId = board.addCircle({
      color: color,
      square: fromSq,
      opacity: 0.2,
      size: 0.8
    })
    element.circleId2 = board.addCircle({
      color: color,
      square: toSq,
      opacity: 0.2,
      size: 0.8
    })

    // Create a new piece with a label of toSq
    var piece = document.createElement('div');
    piece.className = 'circle-piece'; //maybe better name is : circle-annotation
    piece.textContent = toSq + '%';
    const square = element.querySelector(`[data-square-coord="${toSq}"]`);
    //console.log(square);    
    square.appendChild(piece);
  }
}

export function setArrowHighlighting(element, fromSq, toSq, color) {

  let board = element.board
  //add fromSq to toSq arrow
  if (board && fromSq && toSq) {
    if (element.ponderArrowId) {
      board.removeArrow(element.ponderArrowId);
      element.ponderArrowId = null;
    }
    //arrow4Id = board.addArrow(fromSq + '-' + toSq, 'green', 'large')
    element.ponderArrowId = board.addArrow({
      color: color,
      end: toSq,
      opacity: 70,
      //size: 'small',
      start: fromSq
    })
  }
}

export function setDoubleArrowHighlighting(element, fromSq, toSq, oppFromSq, oppToSq, color, oppColor) {
  try {
    let board = element.board
    //add fromSq to toSq arrow
    if (board && fromSq && toSq) {
      if (element.ponderArrowId) {
        board.removeArrow(element.ponderArrowId);
        element.ponderArrowId = null;
      }
      if (element.arrow2Id) {
        board.removeArrow(element.arrow2Id);
        element.arrow2Id = null;
      }
      //arrow4Id = board.addArrow(fromSq + '-' + toSq, 'green', 'large')
      element.ponderArrowId = board.addArrow({
        color: color,
        end: toSq,
        opacity: 70,
        //size: 'small',
        start: fromSq
      })
      if (oppFromSq && oppToSq) {
        element.arrow2Id = board.addArrow({
          color: oppColor,
          end: oppToSq,
          opacity: 70,
          //size: 'small',
          start: oppFromSq
        })

        //board.addArrow(oppFromSq + '-' + oppToSq, oppColor)
      }
    }
  } catch (error) {
    console.error('An error occurred in setDoubleArrowHighlighting:', error);
    // Handle the error gracefully, such as logging the error or displaying an error message to the user.
  }
}

// Call this once after your board is rendered to create the overlay layer.
export function initializeOverlayLayer(boardContainer) {
  boardContainer.style.position = 'relative';

  let overlayLayer = boardContainer.querySelector('.overlay-layer');
  if (!overlayLayer) {
    overlayLayer = document.createElement('div');
    overlayLayer.className = 'overlay-layer';
    overlayLayer.style.position = 'absolute';
    overlayLayer.style.top = '0';
    overlayLayer.style.left = '0';
    overlayLayer.style.width = '100%';
    overlayLayer.style.height = '100%';
    overlayLayer.style.pointerEvents = 'none'; // allow clicks to pass through
    boardContainer.appendChild(overlayLayer);
  }
}

// Function to add a label overlay to a specific square.
export function addOverlayLabel(boardContainer, square, text) {
  const overlayLayer = boardContainer.querySelector('.overlay-layer');
  if (!overlayLayer) {
    console.error('Overlay layer not found. Call initializeOverlayLayer first.');
    return;
  }

  // Check if an overlay for the given square already exists.
  const existingOverlay = overlayLayer.querySelector(`.square-overlay[data-square="${square}"]`);
  if (existingOverlay) {
    // Optionally update the text if needed.
    //existingOverlay.textContent = text;
    return;
  }

  // Get the square element on the board using the data attribute
  const squareElement = boardContainer.querySelector(`[data-square-coord="${square}"]`);
  if (!squareElement) {
    console.error(`Square ${square} not found on board.`);
    return;
  }

  // Calculate square's position relative to the board container.
  const squareRect = squareElement.getBoundingClientRect();
  const boardRect = boardContainer.getBoundingClientRect();

  // Create the overlay element.
  const overlay = document.createElement('div');
  overlay.className = 'square-overlay';
  overlay.setAttribute('data-square', square); // Mark which square this overlay belongs to
  overlay.textContent = text;
  overlay.style.position = 'absolute';
  overlay.style.left = (squareRect.left - boardRect.left) + 'px';
  overlay.style.top = (squareRect.top - boardRect.top) + 'px';
  overlay.style.width = squareRect.width + 'px';
  overlay.style.height = squareRect.height + 'px';
  overlay.style.display = 'flex';
  overlay.style.alignItems = 'center';
  overlay.style.justifyContent = 'center';
  overlay.style.color = '#FFEB3B';
  overlay.style.fontWeight = 'bold';
  overlay.style.fontSize = '12px';  // Smaller text size
  overlay.style.zIndex = '101';
  overlayLayer.appendChild(overlay);
}

export function clearOverlayLabels(boardContainer) {
  const board = boardContainer.board;
  if (!board) {
    console.error('Board is not initialized.');
    return;
  }
  if (boardContainer.circles) {
    boardContainer.circles.forEach(circle => board.removeCircle(circle));
  }

  // Clear existing arrows.
  if (boardContainer.arrowIds) {
    boardContainer.arrowIds.forEach(arrowId => board.removeArrow(arrowId));
  }
  const overlayLayer = boardContainer.querySelector('.overlay-layer');
  if (overlayLayer) {
    overlayLayer.innerHTML = '';
  }
}

// Update your arrow sequence function to work with the overlay layer.
export function setArrowSequence(boardContainer, moves) {
  const board = boardContainer.board;
  if (!board) {
    console.error('Board is not initialized.');
    return;
  }

  if (boardContainer.circles) {
    boardContainer.circles.forEach(circle => board.removeCircle(circle));
  }  

  // Clear existing arrows.
  if (boardContainer.arrowIds) {
    boardContainer.arrowIds.forEach(arrowId => board.removeArrow(arrowId));
  }

  boardContainer.arrowIds = [];
  boardContainer.circles = []

  // Clear all existing overlays from the dedicated overlay layer.
  const overlayLayer = boardContainer.querySelector('.overlay-layer');
  if (overlayLayer) {
    overlayLayer.innerHTML = '';
  }

  moves.forEach(move => {
    const { fromSq, toSq, text } = move;
    if (fromSq && toSq && text) {
      const arrowId = board.addArrow({
        color: 'black',
        start: fromSq,
        end: toSq,
        opacity: 0.25,
        size: 0.3
      });
      boardContainer.arrowIds.push(arrowId);

      const circleId = board.addCircle({
        color: '#3C3C3C',
        square: toSq,
        opacity: 0.80,
        size: 0.45
      });

      boardContainer.circles.push(circleId);

      // Add overlay label to the target square.
      addOverlayLabel(boardContainer, toSq, text);
    }
  });
}

export function setPonderArrowHighlightingWithLabel(element, text, fromSq, toSq, arrowColor, textColor, bgColor) {
  let board = element.board
  //add fromSq to toSq arrow
  if (board && fromSq && toSq) {
    if (element.ponderArrowId) {
      board.removeArrow(element.ponderArrowId);
      element.ponderArrowId = null;
      //console.log('done with removing');      
    }
    //arrow4Id = board.addArrow(fromSq + '-' + toSq, 'green', 'large')
    element.ponderArrowId = board.addArrow({
      color: arrowColor,
      end: toSq,
      opacity: 70,
      //size: 'small',
      start: fromSq
    })
    //let log = 'added arrow setPonderArrowHighlightingWithLabel with id: ' + element.ponderArrowId;
    //console.log(log);
    setLabel(element, toSq, text, textColor, bgColor);
  }
}


export function setPVBoardWithSquareHighlighting(element, fen, color, fromSq, toSq, withHighLight) {
  try {

    if (element.board) {
      element.board.setPosition(fen);
    }
    else {
      createChessboard(element, fen);
      console.log('element.board is undefined');
    }

    if (withHighLight && fromSq) {
      highlightSquare2(element, fromSq, color, true);
      highlightSquare2(element, toSq, color, false);
    }
    else {
      clearHighlightSquaresForElement(element);
    }
  } catch (error) {
    console.error('An error occurred in setBoardWithHighlighting:', error);
    // Handle the error gracefully, such as logging the error or displaying an error message to the user.
  }
}

export function setBoardWithSquareHighlighting(element, fen, color, fromSq, toSq, withHighLight) {
  try {
    if (element.board) {
      if (element.arrow2Id) {
        element.board.removeArrow(element.arrow2Id);
        element.arrow2Id = null;
      }
      if (element.ponderArrowId) {
        //let log = 'remove ponder arrow in setBoardWithSquareHighlighting: ' + element.ponderArrowId;
        //console.log(log);
        element.board.removeArrow(element.ponderArrowId);
        element.ponderArrowId = null;
      }
      removeCirclePieces();
      // Check if the element or the board is undefined here before setting the position
      element.board.position(fen, false);
    }

    else {
      console.log('element.board is undefined');
    }

    if (withHighLight) {
      highlightSquare2(element, fromSq, color, true);
      highlightSquare2(element, toSq, color, false);
    }
  } catch (error) {
    console.error('An error occurred in setBoardWithHighlighting:', error);
    // Handle the error gracefully, such as logging the error or displaying an error message to the user.
  }
}

export function puzzleBoardToElement(element, fen, notation, fromSq, toSq, wFromSq, wToSq) {
  var config = {
    showNotation: notation,
    position: fen,
    useAnimation: false
  }

  var board = Chessboard2(element, config);
  //add fromSq to toSq arrow
  if (fromSq && toSq) {
    let arrow2Id = board.addArrow(fromSq + '-' + toSq, 'green')
  }

  if (wFromSq && wToSq) {
    let arrow4Id = board.addArrow(wFromSq + '-' + wToSq, 'crimson')
  }
}

export function addChessboardToElement(dotnetHelper, fen, element) {
  var legal = false;

  var config = {
    showNotation: false,
    draggable: true,
    position: fen,
    onDragStart: onDragStart,
    onDrop: onDrop,
  }

  var board = Chessboard2(element, config)
  element.board = board;
  initializeOverlayLayer(element);

  async function onDragStart(dragStartEvt) {
    element.querySelector(`[data-square-coord=${dragStartEvt.square}]`).style.opacity = .999
    let toMove = await dotnetHelper.invokeMethodAsync('sideToMove');
    let square = dragStartEvt.square;
    legal = await dotnetHelper.invokeMethodAsync('isLegalPieceMove', square);
    if (legal === false) {
      return false
    }
    clearHighlightSquaresForElement(element);
    let fromSq = dragStartEvt.square;
    highlightSquare2(element, fromSq, toMove, false);

  }

  async function onDrop(dropEvt) {
    element.querySelector(`[data-square-coord=${dropEvt.source}]`).style.opacity = 1
    let fromSq = dropEvt.source;
    let target = dropEvt.target;
    let moveStr = fromSq + target;
    legal = await dotnetHelper.invokeMethodAsync('isLegalMove', moveStr);

    // illegal move
    if (legal === false) {
      var fen = await dotnetHelper.invokeMethodAsync('GetPositionFen');
      board.position(fen, true);
      return;
    }
    let toMove = await dotnetHelper.invokeMethodAsync('sideToMove');
    let moveValidated = await dotnetHelper.invokeMethodAsync('GetMoveStr', moveStr);
    // Check if this is a promotion move
    if (moveValidated && moveValidated.startsWith("promotion:")) {

      // Get the position of the target square for showing the dialog
      const square = element.querySelector(`[data-square-coord=${target}]`);
      const rect = square.getBoundingClientRect();

      // Get the side to move to determine piece color
      const isWhite = toMove === 'w';

      // Show promotion dialog and get selected piece
      const promotionPiece = await dotnetHelper.invokeMethodAsync('ShowPromotionDialog', moveStr, Math.round(rect.left), Math.round(rect.top), isWhite);

      // Now we have the complete move with promotion piece
      dotnetHelper.invokeMethodAsync('UpdateNewMove', promotionPiece, true);
    } else {
      dotnetHelper.invokeMethodAsync('UpdateNewMove', moveValidated, true);
    }

    var fen = await dotnetHelper.invokeMethodAsync('GetPositionFen');
    board.position(fen, true);
    highlightSquare2(element, target, toMove, false);
  }

  return board;
}

export function setPositionWithCallback(dotnetHelper, element, fen, withCallback) {
  let board = element.board;
  board.position(fen, true);
  clearHighlightSquaresForElement(element);
  if (withCallback) {
    dotnetHelper.invokeMethodAsync('UpdateNewMove', "", true);
  }
}

export function addPolicyCircles(element, moves) {
  setArrowSequence(element, moves);  
}

export function makeSimpleMove2(element, color, fromSq, toSq, fen) {
  let board = element.board;
  board.position(fen, true);
  clearHighlightSquaresForElement(element);
  highlightSquare2(element, fromSq, color, false);
  highlightSquare2(element, toSq, color, false);
}


export function setSimplePosition(element, fen) {
  element.board.position(fen, false);
  clearHighlightSquaresForElement(element);
  clearOverlayLabels(element);
}

export function rotateBoard(element) {
  let board = element.board;
  board.flip();
  clearHighlightSquaresForElement(element);
  clearOverlayLabels(element);
}
export function setLineChartData(chart, layout, config) {
  try {
    var data = [config.trace1, config.trace2, config.trace3];
    Plotly.newPlot(chart, data, layout, { displayModeBar: false });
  } catch (error) {
    console.error(error);
  }
}

export function setSingleNodeChart(chart, layout, config) {
  try {
    var data = [config.trace1, config.trace2];
    Plotly.newPlot(chart, data, layout, { displayModeBar: false });
  } catch (error) {
    console.error(error);
  }
}

export function setDoubleNodeChart(chart, layout, config) {
  try {
    var data = [config.trace1, config.trace2];
    Plotly.newPlot(chart, data, layout, { displayModeBar: false });
  } catch (error) {
    console.error(error);
  }
}

export function setLineEvalChartData(chart, layout, config) {
  try {
    var data = [config.trace1, config.trace2];
    Plotly.newPlot(chart, data, layout, { displayModeBar: false });
  } catch (error) {
    console.error(error);
  }
}

export function updateLineEvalChartData(chart, trace, index) {
  try {
    var x = trace.x;
    var y = trace.y;
    var x0 = [[x[x.length - 1]]];
    var y0 = [[y[y.length - 1]]];
    Plotly.extendTraces(chart, { x: x0, y: y0 }, [index]);
  } catch (error) {
    console.error(error);
  }
}

export function setTimeUsageChartData(chart, layout, config) {
  try {
    var data = [config.trace1, config.trace2];
    Plotly.newPlot(chart, data, layout, { displayModeBar: false });
  } catch (error) {
    console.error(error);
  }
}

export function setQdataPlot(chart, layout, arr) {
  var data = arr
  Plotly.newPlot(chart, data, layout, { responsive: true, displayModeBar: false });
}

export function setNdataPlot(chart, layout, data) {
  Plotly.newPlot(chart, data, layout, { responsive: true, displayModeBar: false });
}

export function updateQdataPlot(chart, trace, index) {
  try {
    var x = trace.x;
    var y = trace.y;
    var x0 = [[x[x.length - 1]]];
    var y0 = [[y[y.length - 1]]];
    Plotly.extendTraces(chart, { x: x0, y: y0 }, [index]);
  } catch (error) {
    console.error(error);
  }
}

export function clearQPlot(chart, layout) {
  Plotly.react(chart, [], layout, { responsive: true, displayModeBar: false });
}

export function setPgnEvalPlot(dotnetHelper, chart, layout, data) {

  Plotly.newPlot(chart, data, layout, { responsive: true });
  chart.on('plotly_click', function (data) {
    var pts = '';
    for (var i = 0; i < data.points.length; i++) {
      pts = data.points[i].x +
        ',' + data.points[i].y.toPrecision(4);
    }
    dotnetHelper.invokeMethodAsync('UpdatePlotInfo', pts);
  });
}

export function changeColorInEvalPlot(chart, data) {
  var X = [data.x - 1];
  var Y = [data.y];

  var l = chart.data.length;
  if (l > 2) {
    Plotly.deleteTraces(chart, 2);
  }

  Plotly.addTraces(chart, {
    x: X,
    y: Y,
    type: 'bar',
    mode: 'markers',
    marker: { 'color': 'red' },
    showlegend: false,
  });
}

export function openBrowserWindow(content) {
  var doc = window.open().document;
  doc.write("<pre>");
  doc.write(content);
  doc.write(" </pre>");
}

export function scrollToEnd(textarea) {
  textarea.scrollTop = textarea.scrollHeight;
}

export function scrollDivToTop(div) {
    var element = document.getElementById(div);
    if (element) {
        element.scrollTop = 0; // Scroll the div element to the top
    }
}

export function scrollDivToEnd(div) {
  var element = document.getElementById(div);
  // Scroll the div element to the last child element  
    if (element) {
        element.scrollTop = element.scrollHeight;
    }
}

// Function to show promotion dialog
export function showPromotionDialog(x, y, isWhite) {
  const dialog = document.getElementById('promotionDialog');
  if (!dialog) return;

  // Position the dialog at the specified coordinates
  dialog.style.left = x + 'px';
  dialog.style.top = y + 'px';

  // Change the piece images based on the side to move
  const color = isWhite ? 'w' : 'b';
  document.getElementById('promoQ').src = `chessboardjs/img/chesspieces/wikipedia/${color}Q.png`;
  document.getElementById('promoR').src = `chessboardjs/img/chesspieces/wikipedia/${color}R.png`;
  document.getElementById('promoB').src = `/chessboardjs/img/chesspieces/wikipedia/${color}B.png`;
  document.getElementById('promoN').src = `/chessboardjs/img/chesspieces/wikipedia/${color}N.png`;

  // Show the dialog
  dialog.style.display = 'block';
}

// Function to hide promotion dialog
export function hidePromotionDialog() {
  const dialog = document.getElementById('promotionDialog');
  if (dialog) {
    dialog.style.display = 'none';
  }
}

