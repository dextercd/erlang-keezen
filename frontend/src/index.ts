const canvas = document.getElementById('game-canvas') as HTMLCanvasElement | null;

if (canvas === null) {
  throw new Error("Couldn't find the canvas.");
}

const context = canvas.getContext('2d');

if (context == null) {
  throw new Error("Couldn't get rendering context.");
}

document.body.scrollTop = 176;


context.fillStyle = '#23161a';
context.fillRect(0, 0, 240, 176);

context.fillStyle = 'green'
context.fillRect(0, 176, 240, 176);


context.fillStyle = '#f9cd81';

const sizey = 150;
const sizex = 200;
const coordx = (240 - sizex) / 2;
const coordy = (176 - sizey) / 2;

context.fillRect(coordx, coordy, sizex, sizey);

const drawPawn = function(colour: string, posx: number, posy: number) {
  context.fillStyle = colour;

  context.fillRect(posx - 2, posy - 3, 6, 3);
  context.fillRect(posx - 1, posy - 5, 4, 2);
  context.fillRect(posx, posy - 6, 2, 1);
  context.fillRect(posx - 1, posy - 8, 4, 2);
  context.fillRect(posx, posy - 9, 2, 1);
}

const drawDivot = function(posx: number, posy: number) {
  context.fillStyle = '#592821';

  context.fillRect(posx - 3, posy - 1, 8, 3);
  context.fillRect(posx - 4, posy, 10, 1);
}

let count = 0;
for (let x = 30; x <= 220; x += 13) {
  for (let y = 20; y <= 155; y += 7) {
    drawDivot(x, y)
    count++
  }
}

drawDivot(70, 70);
drawPawn('green', 70, 70);
drawPawn('deeppink', 80, 70);
drawPawn('blue', 90, 70);
drawPawn('black', 100, 70);
