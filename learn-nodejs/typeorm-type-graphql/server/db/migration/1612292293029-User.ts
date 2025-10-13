import {MigrationInterface, QueryRunner} from "typeorm";

export class User1612292293029 implements MigrationInterface {
    name = 'User1612292293029'

    public async up(queryRunner: QueryRunner): Promise<void> {
        await queryRunner.query(`CREATE TABLE "public"."user" ("id" uuid NOT NULL DEFAULT uuid_generate_v4(), "created_at" TIMESTAMP NOT NULL DEFAULT now(), "updated_at" TIMESTAMP NOT NULL DEFAULT now(), "username" character varying NOT NULL, "password" character varying NOT NULL, CONSTRAINT "UQ_b67337b7f8aa8406e936c2ff754" UNIQUE ("username"), CONSTRAINT "PK_03b91d2b8321aa7ba32257dc321" PRIMARY KEY ("id"))`);
    }

    public async down(queryRunner: QueryRunner): Promise<void> {
        await queryRunner.query(`DROP TABLE "public"."user"`);
    }

}
