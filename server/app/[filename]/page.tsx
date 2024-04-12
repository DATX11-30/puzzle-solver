"use client";

import Sudoku from "@/components/sudoku/sudoku";
import { getSolution } from "@/lib/getSolution";
import { useParams } from "next/navigation";
import React, { useEffect, useState } from "react";

type Props = {};

const page = (props: Props) => {
  const { filename } = useParams();
  const [partialSuds, setPartialSuds] = useState<string[] | null>(null);
  const [techniques, setTechniques] = useState<string[] | null>(null);
  const [currentIndex, setCurrentIndex] = useState(0);

  const handleKeyDown = (event: KeyboardEvent) => {
    if (event.key == "ArrowRight") {
      setCurrentIndex((prev) => prev + 1);
    } else if (event.key == "ArrowLeft") {
      setCurrentIndex((prev) => prev - 1);
    }
  };

  useEffect(() => {
    getSolution(filename as string).then((solution) => {
		console.log(solution);

      setPartialSuds(solution.partialSuds);
	  setTechniques(solution.techniques);
    });
  }, []);

  useEffect(() => {
    window.addEventListener("keydown", handleKeyDown);

    return () => {
      window.removeEventListener("keydown", handleKeyDown);
    };
  }, []);

  if (!partialSuds || !techniques) {
    return <div>Loading...</div>;
  }

  return (
    <div>
      <p>{currentIndex + 1} / {partialSuds.length}</p>
	  <Sudoku sudoku={partialSuds[currentIndex]} />
	  <p>{techniques[currentIndex]}</p>
    </div>
  );
};

export default page;
