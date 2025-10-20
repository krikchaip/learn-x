import type { NextFunction, Request, Response } from 'express';

export function HeaderMiddleware(header: Record<string, string>) {
  return function (_: Request, res: Response, next: NextFunction) {
    res.setHeaders(new Map(Object.entries(header)));
    next();
  };
}
