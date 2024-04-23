FROM haskell:latest
WORKDIR /app
COPY ./src ./src
RUN ghc -o out ./src/main.hs -i ./src/io.hs ./src/logic.hs ./src/optimizer.hs ./src/solver.hs ./src/sudoku.hs ./src/sudokulogic.hs ./src/sudokus.hs

FROM node:latest
WORKDIR /app
COPY ./server ./
COPY --from=0 /app/out ./out
COPY sudokus ./sudokus
RUN npm i
RUN npm run build
CMD ["npm", "run", "start"]
EXPOSE 3000