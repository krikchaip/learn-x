const IS_PRODUCTION = process.env['IS_PRODUCTION'] === 'true';
const IS_OFFLINE = process.env['IS_OFFLINE'] === 'true';

const DYNAMODB_CUSTOMER_TABLE = process.env['DYNAMODB_CUSTOMER_TABLE'];

export const environment = {
  production: IS_PRODUCTION,
  offline: IS_OFFLINE,
  customer: {
    table: DYNAMODB_CUSTOMER_TABLE,
  },
};
