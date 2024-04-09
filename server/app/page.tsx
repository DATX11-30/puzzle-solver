"use client"

import { getSolutionFromFile, getAllSudokus } from "@/lib/getSolution";
import { useEffect, useState } from "react";

export default function Home() {
  const [state, setState] = useState<String | null>(null);
  const [sudokus, setSudokus] = useState<any>(null);

  useEffect(() => {
    getSolutionFromFile("../sudokus/Beginner/Beginner_2024_01_01.txt").then((solution) => {
      if (!solution) {
        setState("No solution found");
      }
      setState(solution);
    });

    getAllSudokus().then((sudokus) => {
      if (!sudokus) {
        setSudokus("No sudokus found");
      }
      setSudokus(sudokus);
    });

    return () => {
      setState(null);
      setSudokus(null);
    };
  }, []);

  if(!state || !sudokus) return (
    <main className="flex min-h-screen flex-col items-center justify-between p-24">
      <h1 className="text-4xl font-bold">Solution</h1>
      <p>Loading...</p>
    </main>
  );
      
  return (

    <main className="flex min-h-screen flex-col items-center justify-between p-24">
      <h1 className="text-4xl font-bold">Solution</h1>
      <p>{state}</p>
      {sudokus.Beginner.map((sudoku: string) => (
        <p key={sudoku}>{sudoku}</p>
      ))}

    </main>
  );
}
