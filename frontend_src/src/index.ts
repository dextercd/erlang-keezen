import './polyfills'

const canvas = document.getElementById('game-canvas') as HTMLCanvasElement | null;

if (canvas === null) {
  throw new Error("Couldn't find the canvas.");
}

const context = canvas.getContext('2d');

if (context === null) {
  throw new Error("Couldn't get rendering context.");
}

canvas.width = canvas.clientWidth;
canvas.height = canvas.clientHeight;
context.scale(canvas.clientWidth / 240, canvas.clientHeight / 352);

document.body.scrollTop = 176;

context.fillStyle = '#23161a';
context.fillRect(0, 0, 240, 176);

context.fillStyle = 'green'
context.fillRect(0, 176, 240, 176);

const sizey = 150;
const sizex = 200;
const coordx = (240 - sizex) / 2;
const coordy = (176 - sizey) / 2;

const drawPlate = function() {
  context.fillStyle = '#f9cd81';
  context.fillRect(coordx, coordy, sizex, sizey);
}

const drawPawn = function(colour: string, posx: number, posy: number) {
  context.fillStyle = colour;

  context.fillRect(posx - 2, posy - 3, 6, 3);
  context.fillRect(posx - 1, posy - 5, 4, 2);
  context.fillRect(posx, posy - 6, 2, 1);
  context.fillRect(posx - 1, posy - 8, 4, 2);
  context.fillRect(posx, posy - 9, 2, 1);
}

const drawDivot = function(posx: number, posy: number, colour: string = '#b28c4a') {
  context.fillStyle = colour;

  context.fillRect(posx - 3, posy - 2, 8, 3);
  context.fillRect(posx - 4, posy - 1, 10, 1);
}

function generateBoardPositions(): number[][] {
  const positions: number[][] = [];
  let x = 40, y = 50;

  const moveSteps = function(count: number, dirx: number, diry: number) {
    while(count-- > 0) {
      positions.push([x, y]);
      x += dirx;
      y += diry;
    }
  }

  const xstraight = 12;
  const xdiagonal = 7;
  const ystraight = 8;
  const ydiagonal = 6;

  moveSteps(4, +xdiagonal, -ydiagonal);
  moveSteps(4, +xdiagonal, +ydiagonal);
  moveSteps(4, +xstraight,  0);
  moveSteps(4, +xdiagonal, -ydiagonal);
  moveSteps(4, +xdiagonal, +ydiagonal);
  moveSteps(4, -xdiagonal, +ydiagonal);
  moveSteps(4,  0        , +ystraight);
  moveSteps(4, +xdiagonal, +ydiagonal);
  moveSteps(4, -xdiagonal, +ydiagonal);
  moveSteps(4, -xdiagonal, -ydiagonal);
  moveSteps(4, -xstraight,  0);
  moveSteps(4, -xdiagonal, +ydiagonal);
  moveSteps(4, -xdiagonal, -ydiagonal);
  moveSteps(4, +xdiagonal, -ydiagonal);
  moveSteps(4,  0        , -ystraight);
  moveSteps(4, -xdiagonal, -ydiagonal);

  return positions;
}

const boardPositions = generateBoardPositions();

const playerPositions = [boardPositions[0], boardPositions[16], boardPositions[32], boardPositions[48]];
const playerColours = ['green', 'deeppink', 'blue', '#333']

let counter = 0;

drawPlate()

for(const [x, y] of boardPositions) {
  drawDivot(x, y);
}

drawDivot(playerPositions[0][0], playerPositions[0][1], 'darkgreen')
drawDivot(playerPositions[1][0], playerPositions[1][1], 'purple')
drawDivot(playerPositions[2][0], playerPositions[2][1], 'lightblue')
drawDivot(playerPositions[3][0], playerPositions[3][1], 'black')

const playCard = (suite: string, rank: string) => () => {
  const playCardRequest = new XMLHttpRequest();
  playCardRequest.open('POST', '/game', true);
  playCardRequest.send(JSON.stringify([suite, rank]));
}

const playerRequest = new XMLHttpRequest();
playerRequest.open('GET', '/game', true);
playerRequest.onload = function(ev) {
  const response = JSON.parse(playerRequest.responseText);
  for (const option of response.options) {
    drawPawn(playerColours[option.pawn.player - 1], boardPositions[option.pawn.position][0], boardPositions[option.pawn.position][1]);
  }

  const cardsContainer = document.getElementById('cards');
  if (cardsContainer === null)
    throw new Error('Error in HTML.');

  cardsContainer.innerHTML = '';
  for (const [suite, rank] of response.player.hand) {
    const button = document.createElement('button');
    button.type = 'button';
    button.innerText = suite + ' ' + rank;
    button.onclick = playCard(suite, rank);

    cardsContainer.appendChild(button);
  }
}
playerRequest.send();
