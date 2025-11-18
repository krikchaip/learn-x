import { DynamoDBClient } from '@aws-sdk/client-dynamodb';
import { DynamoDBDocumentClient, PutCommand } from '@aws-sdk/lib-dynamodb';
import {
  APIGatewayProxyEvent,
  APIGatewayProxyResult,
  Context,
} from 'aws-lambda';
import { randomBytes } from 'node:crypto';

import { environment } from '../../environments/environment';

const client = DynamoDBDocumentClient.from(new DynamoDBClient());

export const lambda = async (
  event: APIGatewayProxyEvent,
  context: Context
): Promise<APIGatewayProxyResult> => {
  const payload: CustomerCreateRequest = JSON.parse(event.body ?? '{}');

  const response = await client.send(
    new PutCommand({
      TableName: environment.customer.table,
      Item: { ...payload, ID: randomBytes(16).toString('hex') },
    })
  );

  console.log(context);

  return {
    statusCode: 201,
    body: JSON.stringify(response),
  };
};
