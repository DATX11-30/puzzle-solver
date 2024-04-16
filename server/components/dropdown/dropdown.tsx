"use client";

import { getSudokus } from "@/lib/getSudokus";
import Droplist from "./droplist";
import { useEffect, useState } from "react";

export default function Dropdown({
  difficulty,
}: {
  difficulty: "Beginner" | "Easy" | "Medium" | "Tricky" | "Fiendish";
}) {
  const [sudokus, setSudokus] = useState<{ filename: string; date: string, hasSolution: boolean }[]>(
    []
  );

  useEffect(() => {
    console.log("useEffect");

    getSudokus(difficulty).then((sudokus) => {
      setSudokus(sudokus);
    });

    return () => setSudokus((prev) => []);
  }, []);

  return (
    <details>
      <summary role="button">{difficulty}</summary>
      <div className="center grid">
        <Droplist items={sudokus} />
      </div>
    </details>
  );
}
