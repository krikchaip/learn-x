import { DynamoDBClient } from '@aws-sdk/client-dynamodb';
import { DynamoDBDocumentClient, ScanCommand } from '@aws-sdk/lib-dynamodb';
import { APIGatewayProxyResult } from 'aws-lambda';

import { environment } from 'src/environments/environment';

const client = DynamoDBDocumentClient.from(new DynamoDBClient());

export const lambda = async (): Promise<APIGatewayProxyResult> => {
  const response = await client.send(
    new ScanCommand({ TableName: environment.customer.table })
  );

  if (!response.Count) {
    return { statusCode: 404, body: '' };
  }

  return {
    statusCode: 200,
    body: JSON.stringify({
      total: response.Count,
      items: response.Items,
    }),
  };
};
