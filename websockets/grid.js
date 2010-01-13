var width;
var height;
var cellSize;
var cellCount;
var grid;
var ctx;
var canvas;

var OFF = 0;
var ON = 1;
var DYING = 2;

var isDrawing = false;

function mouseClickChange() {
    isDrawing = !isDrawing;
}

function mouseMove(e){
    if (isDrawing) {
        var x = Math.floor((e.clientX - canvas[0].offsetLeft) / cellSize);
        var y = Math.floor((e.clientY - canvas[0].offsetTop) / cellSize);
        cycle(x,y);
        drawGrid();        
    }
}

function init(cvs) {
    canvas = cvs;
    width = canvas.width();
    height = canvas.height();
    ctx = canvas[0].getContext("2d");
    cellSize = 10;
    
    cellCount = Math.min(width,height) / cellSize;
    
    grid = new Array(cellCount);
    for (i=0;i<cellCount;++i) {
        grid[i] = new Array(cellCount);
        for (j=0;j<cellCount;++j) {
            grid[i][j]=OFF;
        }
    }
}

function cycle(x,y) {
    switch(grid[x][y]) {
    case OFF:
        grid[x][y] = ON;
        break;
    case ON:
        grid[x][y] = DYING;
        break;
    case DYING:
        grid[x][y] = OFF;
        break;
    default:
        throw new Error('Unknown state')
    }
}

function getColors(state) {
    switch(state) {
    case OFF:
        return "#000000";
    case ON:
        return "#FF0000";
    case DYING:
        return "#00FF00";
    default:
        throw new Error('Unknown state');
    }
}

function drawGrid(){
    for (i=0;i<cellCount;++i) {
        for (j=0;j<cellCount;++j) {
            ctx.fillStyle = getColors(grid[i][j]);
            ctx.beginPath();
            ctx.rect(i*cellSize,j*cellSize,cellSize-1,cellSize-1);
            ctx.closePath();
            ctx.fill();
        }
    }
}

function runStep(ws) {
    var cells = '';
    var pos = 0;
    for (i=0;i<cellCount;++i) {
        for (j=0;j<cellCount;++j) {
            cells += grid[i][j];
        }
    }
    ws.send(cellCount + '\n' + cells);
}

// callback will get here.
function updateGrid(data) {
    var pos=0;
    for (i=0;i<cellCount;++i) {
        for (j=0;j<cellCount;++j) {
            grid[i][j] = parseInt(data.charAt(pos++));
            if (grid[i][j] != 0 && grid[i][j] != 1 && grid[i][j] != 2) {
                alert(grid[i][j]);
            }
        }
    }
}